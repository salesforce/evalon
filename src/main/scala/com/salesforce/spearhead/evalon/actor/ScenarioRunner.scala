/*
 * Copyright (c) 2025, Salesforce, Inc.
 * SPDX-License-Identifier: Apache-2
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.salesforce.spearhead.evalon.actor

import io.circe.syntax.*
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

import com.salesforce.spearhead.evalon.agent.Agent
import com.salesforce.spearhead.evalon.llm.Llm
import com.salesforce.spearhead.evalon.model.*

/**
 * The ScenarioRunner orchestrates a simulation as an actor.
 *
 * Each participant is self-driven: they generate responses when messages arrive. The runner
 * delivers messages to direct participants (those in the conversation's `between` list), records
 * the transcript, notifies observers via events, and stops when any participant signals [END] or
 * the message limit is reached.
 */
object ScenarioRunner:

  sealed trait Command

  // External API
  case class Run(replyTo: ActorRef[SimulationResult]) extends Command

  // Responses from participants (sent directly to runner)
  case class ParticipantResponse(
      participant: String,
      conversation: String,
      action: Action
  ) extends Command

  // Event source emissions
  case class EventSourceEmission(sourceName: String, events: List[Event]) extends Command

  case class SimulationResult(transcript: Transcript)

  /** Internal state for the running simulation. */
  private case class RunState(
      scenario: Scenario,
      transcript: Transcript,
      participants: Map[String, ActorRef[Participant.Command]],
      eventSources: Map[String, ActorRef[EventSourceActor.Command]],
      directConversations: Map[String, Set[String]],
      observationsOf: Map[String, List[ObservationConfig]],
      conversations: Map[String, ConversationConfig],
      agentName: String,
      messageCounts: Map[String, Int],
      replyTo: ActorRef[SimulationResult],
      onEntry: Option[TranscriptEntry => Unit]
  )

  def apply(
      scenario: Scenario,
      agent: Agent,
      llm: Llm,
      onEntry: Option[TranscriptEntry => Unit] = None
  ): Behavior[Command] = Behaviors.receive {
    case (ctx, Run(replyTo)) =>
      // Identify the evaluated participant
      val agentName = scenario.participants
        .find(_._2.participantType == ParticipantType.Evaluated)
        .map(_._1)
        .getOrElse("agent")

      // Compute the agent's direct conversations
      val agentDirectConversations = scenario.conversations
        .filter(_.between.contains(agentName))
        .map(_.name)
        .toSet

      // Conversation lookup
      val conversations = scenario.conversations.map(c => c.name -> c).toMap

      // Spawn all participant actors
      val participants: Map[String, ActorRef[Participant.Command]] = scenario.participants.map {
        case (name, config) if config.participantType == ParticipantType.Evaluated =>
          name -> ctx.spawn(
            EvaluatedAgent(agent, ctx.self, agentName, agentDirectConversations),
            s"participant-$name"
          )
        case (name, config) =>
          name -> ctx.spawn(
            SimulatedParticipant(config, llm, ctx.self, conversations),
            s"participant-$name"
          )
      }

      // Spawn event source actors
      val eventSources = scenario.eventSources.map { esConfig =>
        esConfig.name -> ctx.spawn(
          EventSourceActor(esConfig, llm, ctx.self),
          s"event-source-${esConfig.name}"
        )
      }.toMap

      // Compute direct conversations per participant
      val directConversations = scenario.participants.map { (name, _) =>
        name -> scenario.conversations.filter(_.between.contains(name)).map(_.name).toSet
      }

      // Build observation lookup: observed target name (conversation or event source) -> observers
      val observationsOf: Map[String, List[ObservationConfig]] =
        scenario.observations
          .flatMap { obs =>
            obs.observes.map(convName => convName -> obs)
          }
          .groupMap(_._1)(_._2)

      val state = RunState(
        scenario = scenario,
        transcript = Transcript(),
        participants = participants,
        eventSources = eventSources,
        directConversations = directConversations,
        observationsOf = observationsOf,
        conversations = conversations,
        agentName = agentName,
        messageCounts = Map.empty,
        replyTo = replyTo,
        onEntry = onEntry
      )

      // Kick off conversations that have an initiator
      for
        conv <- scenario.conversations
        initiator <- conv.initiatedBy
      do
        participants.get(initiator).foreach { ref =>
          ref ! Participant.ReceiveMessage(
            Message("system", "The conversation is starting. Please begin."),
            conv.name
          )
        }

      running(state)

    case (ctx, msg: ParticipantResponse) =>
      ctx.log.warn("ParticipantResponse before Run, ignoring: {}", msg)
      Behaviors.same
    case (ctx, msg: EventSourceEmission) =>
      ctx.log.warn("EventSourceEmission before Run, ignoring: {}", msg)
      Behaviors.same
  }

  /** Main running behavior — handles responses as they arrive. */
  private def running(state: RunState): Behavior[Command] = Behaviors.receive {
    case (ctx, ParticipantResponse(participant, conversation, action)) =>
      action match
        case Action.End =>
          val finalState = appendEntry(
            state,
            TranscriptEntry(
              conversation = Some(conversation),
              message = Some(Message(participant, Signals.End))
            )
          )
          finalState.replyTo ! SimulationResult(finalState.transcript)
          Behaviors.stopped

        case send: Action.Send =>
          val newState = processSendAction(state, send, conversation)
          // Check message limit per conversation
          val count = newState.messageCounts.getOrElse(conversation, 0)
          if count >= newState.scenario.maxTurns * 2 then
            newState.replyTo ! SimulationResult(newState.transcript)
            Behaviors.stopped
          else running(newState)

    case (_, EventSourceEmission(sourceName, events)) =>
      var s = state
      for event <- events do s = appendEntry(s, TranscriptEntry(event = Some(event)))
      for
        obs <- s.scenario.observations
        if obs.observes.contains(sourceName)
      do
        s.participants.get(obs.participant).foreach { ref =>
          ref ! Participant.ReceiveEvents(events)
        }
      running(s)

    case (ctx, msg: Run) =>
      ctx.log.warn("Run received in running state, ignoring: {}", msg)
      Behaviors.same
  }

  /**
   * Process a Send action: record tool trace and message, deliver to participants, notify
   * observers.
   */
  private def processSendAction(
      state: RunState,
      send: Action.Send,
      conversation: String
  ): RunState =
    // Record tool interactions
    val toolEntries = send.toolTrace.flatMap { interaction =>
      List(
        TranscriptEntry(conversation = Some(conversation), toolCall = Some(interaction.call)),
        TranscriptEntry(conversation = Some(conversation), toolResult = Some(interaction.result))
      )
    }
    val stateAfterTools = toolEntries.foldLeft(state)(appendEntry)

    // Record and deliver message (skip if empty content)
    if send.message.content.isEmpty then stateAfterTools
    else
      val msg = send.message
      val entry = TranscriptEntry(
        conversation = Some(conversation),
        message = Some(msg)
      )
      val stateAfterMsg = appendEntry(stateAfterTools, entry)

      // Deliver to direct participants
      for
        (name, ref) <- stateAfterMsg.participants
        if name != msg.sender
        if stateAfterMsg.directConversations.getOrElse(name, Set.empty).contains(conversation)
      do ref ! Participant.ReceiveMessage(msg, conversation)

      // Notify observers
      val events = List(
        Event(
          "conversation_message",
          Map(
            "conversation" -> conversation.asJson,
            "sender" -> msg.sender.asJson,
            "content" -> msg.content.asJson
          )
        )
      )
      for
        obs <- stateAfterMsg.observationsOf.getOrElse(conversation, Nil)
        if msg.sender != obs.participant
        ref <- stateAfterMsg.participants.get(obs.participant)
      do ref ! Participant.ReceiveEvents(events)

      // Feed to event sources
      for (_, esRef) <- stateAfterMsg.eventSources do
        esRef ! EventSourceActor.SimulationUpdate(entry)

      // Increment message count
      val count = stateAfterMsg.messageCounts.getOrElse(conversation, 0) + 1
      stateAfterMsg.copy(messageCounts = stateAfterMsg.messageCounts + (conversation -> count))

  /** Append an entry to the transcript and invoke the onEntry callback. */
  private def appendEntry(state: RunState, entry: TranscriptEntry): RunState =
    state.onEntry.foreach(_(entry))
    state.copy(transcript = state.transcript.append(entry))
