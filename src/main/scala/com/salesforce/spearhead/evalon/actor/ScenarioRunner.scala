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
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

import com.salesforce.spearhead.evalon.agent.Agent
import com.salesforce.spearhead.evalon.llm.Llm
import com.salesforce.spearhead.evalon.model.*
import com.salesforce.spearhead.evalon.scenario.ScenarioLoader

/**
 * The ScenarioRunner orchestrates a simulation as an actor.
 *
 * Each participant is self-driven: they generate responses when messages arrive. The runner
 * delivers messages to direct participants (those in the conversation's `between` list), records
 * the transcript, notifies observers via events, and stops when any participant signals [END] or
 * the message limit is reached.
 *
 * It also stops when the run goes idle. The runner counts what it delivers to each participant and
 * event source, and each one reports `Idle(processed)` when it has nothing left to do, starting
 * with a report when it starts. One that has never reported counts as busy, so a source that can
 * act on its own simply never reports. When every last-reported count matches what was delivered,
 * nobody is working and nothing is on its way, so nobody will act again. The runner then sends the
 * evaluated agent one `all_waiting` event; if the run goes idle again with nothing new posted, it
 * completes with [[StopReason.Idle]].
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

  // A participant's async work failed terminally (thrown, null/failed CompletionStage, exhausted
  // retries): the evaluated agent's step, a simulated participant's LLM call, or an event source's.
  case class ParticipantFailed(participant: String, cause: Throwable) extends Command

  /**
   * A participant or event source has nothing to do and won't act again unless the runner sends it
   * something new. `processed` counts the runner deliveries it has handled so far, so a report that
   * crossed a newer delivery in flight is recognised as stale. Sent via [[IdleReporter]].
   */
  case class Idle(name: String, processed: Int) extends Command

  /**
   * Sent to the evaluated agent the first time a quiet period is detected. Named for what was
   * detected, not as an instruction; remote agents may match on the name, so treat it as API.
   */
  private[actor] val AllWaitingEvent = Event(
    Event.AllWaiting,
    Map(
      "message" -> ("Every participant is waiting and nothing is in progress. " +
        "The run ends if nothing new is posted.").asJson
    )
  )

  /**
   * Outcome of a simulation. Both arms carry a transcript: `Failed` holds whatever was recorded up
   * to the point a participant failed, and names the participant that failed.
   */
  enum SimulationResult:
    case Completed(transcript: Transcript, stopReason: StopReason)
    case Failed(transcript: Transcript, cause: Throwable, participant: String)

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
      onEntry: Option[TranscriptEntry => Unit],
      // Idle detection: deliveries per participant/event source, the count each last reported when
      // it went idle, and whether the agent was sent all_waiting with nothing posted since.
      delivered: Map[String, Int] = Map.empty,
      reported: Map[String, Int] = Map.empty,
      idleNotified: Boolean = false
  )

  /**
   * Throws IllegalArgumentException for a scenario that fails [[ScenarioLoader.validateScenario]].
   * Checked here, not only in EvalonRunner, because direct callers skip EvalonRunner; and when the
   * behavior is built, not when it starts, so a caller isn't left waiting on a runner that never
   * replies.
   */
  def apply(
      scenario: Scenario,
      agent: Agent,
      llm: Llm,
      onEntry: Option[TranscriptEntry => Unit] = None
  ): Behavior[Command] =
    ScenarioLoader.validateScenario(scenario).left.foreach { e =>
      throw IllegalArgumentException(s"Invalid scenario: $e")
    }
    run(scenario, agent, llm, onEntry)

  private def run(
      scenario: Scenario,
      agent: Agent,
      llm: Llm,
      onEntry: Option[TranscriptEntry => Unit]
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
            EvaluatedAgent(agent, ctx.self, agentName, agentDirectConversations, name),
            s"participant-$name"
          )
        case (name, config) =>
          // The map key is the participant's identity everywhere else (routing, conversations,
          // delivery counts), so it overrides a config built in code with a different name.
          name -> ctx.spawn(
            SimulatedParticipant(config.copy(name = name), llm, ctx.self, conversations),
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
      val kickoffs = for
        conv <- scenario.conversations
        initiator <- conv.initiatedBy
        ref <- participants.get(initiator)
      yield (initiator, ref, conv.name)
      val started = kickoffs.foldLeft(state) { case (s, (initiator, ref, conv)) =>
        deliver(
          s,
          initiator,
          ref,
          Participant.Kickoff(conv)
        )
      }

      // Actors report idle when they start, so a run with nothing kicked off is detected then. With
      // no actors there is nobody to report; checking now ends that run. Existing actors haven't
      // reported yet, so they still count as busy and this changes nothing for them.
      checkIdle(ctx, started)

    case (ctx, msg: ParticipantResponse) =>
      ctx.log.warn("ParticipantResponse before Run, ignoring: {}", msg)
      Behaviors.same
    case (ctx, msg: EventSourceEmission) =>
      ctx.log.warn("EventSourceEmission before Run, ignoring: {}", msg)
      Behaviors.same
    case (ctx, msg: ParticipantFailed) =>
      ctx.log.warn("ParticipantFailed before Run, ignoring: {}", msg)
      Behaviors.same
    case (ctx, msg: Idle) =>
      ctx.log.warn("Idle before Run, ignoring: {}", msg)
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
          complete(finalState, StopReason.End)

        case send: Action.Send =>
          val newState = processSendAction(state, send, conversation)
          // Check message limit per conversation
          val count = newState.messageCounts.getOrElse(conversation, 0)
          if count >= newState.scenario.maxTurns * 2 then complete(newState, StopReason.MaxTurns)
          else running(newState)

    case (ctx, EventSourceEmission(sourceName, emitted)) =>
      // An event source's LLM can return any name, so drop the harness's own: an emitted
      // all_waiting or conversation_message would be indistinguishable from the real signal.
      val (spoofed, events) = emitted.partition(e => Event.ReservedNames.contains(e.name))
      if spoofed.nonEmpty then
        ctx.log.warn(
          "Dropping event(s) with reserved names from event source {}: {}",
          sourceName,
          spoofed.map(_.name).mkString(", ")
        )
      if events.isEmpty then running(state)
      else
        val recorded = events.foldLeft(state.copy(idleNotified = false)) { (s, event) =>
          appendEntry(s, TranscriptEntry(event = Some(event)))
        }
        val observers = for
          obs <- recorded.scenario.observations
          if obs.observes.contains(sourceName)
          ref <- recorded.participants.get(obs.participant)
        yield obs.participant -> ref
        running(observers.foldLeft(recorded) { case (s, (name, ref)) =>
          deliver(s, name, ref, Participant.ReceiveEvents(events))
        })

    case (ctx, Idle(name, processed)) =>
      checkIdle(ctx, state.copy(reported = state.reported + (name -> processed)))

    case (_, ParticipantFailed(participant, cause)) =>
      // Fail the whole simulation fast, carrying the partial transcript for debugging.
      state.replyTo ! SimulationResult.Failed(state.transcript, cause, participant)
      Behaviors.stopped

    case (ctx, msg: Run) =>
      ctx.log.warn("Run received in running state, ignoring: {}", msg)
      Behaviors.same
  }

  private def complete(state: RunState, reason: StopReason): Behavior[Command] =
    state.replyTo ! SimulationResult.Completed(state.transcript, reason)
    Behaviors.stopped

  /**
   * If every participant and event source has reported handling everything delivered to it, nobody
   * will act again. The first time, wake the agent with all_waiting; if the run is idle again with
   * nothing new posted since, complete it.
   */
  private def checkIdle(ctx: ActorContext[Command], state: RunState): Behavior[Command] =
    val names = state.participants.keySet ++ state.eventSources.keySet
    if !allIdle(names, state.delivered, state.reported) then running(state)
    else
      state.participants.get(state.agentName) match
        // An agent with no conversation of its own has nowhere to answer the event.
        case Some(agent)
            if !state.idleNotified
              && state.directConversations.getOrElse(state.agentName, Set.empty).nonEmpty =>
          // A harness signal, not part of the scenario, so it stays out of the transcript.
          ctx.log.info("Run is idle; sending {} all_waiting", state.agentName)
          val notified = state.copy(idleNotified = true)
          running(
            deliver(
              notified,
              state.agentName,
              agent,
              Participant.ReceiveEvents(List(AllWaitingEvent))
            )
          )
        case _ =>
          ctx.log.info("Run is still idle after all_waiting; completing")
          complete(state, StopReason.Idle)

  /**
   * Whether every named actor's last report matches what was delivered to it. An actor that has
   * never reported counts as busy, even with nothing delivered: it may be acting on its own.
   */
  private[actor] def allIdle(
      names: Set[String],
      delivered: Map[String, Int],
      reported: Map[String, Int]
  ): Boolean =
    names.forall(name => reported.get(name).contains(delivered.getOrElse(name, 0)))

  /** Send a command to a participant or event source, counting it toward idle detection. */
  private def deliver[T](state: RunState, name: String, ref: ActorRef[T], command: T): RunState =
    ref ! command
    state.copy(delivered = state.delivered.updated(name, state.delivered.getOrElse(name, 0) + 1))

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
    val toolEntries = send.toolInteractions.flatMap { interaction =>
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
      // Something new was posted, so a later quiet period gets its own all_waiting.
      val stateAfterMsg = appendEntry(stateAfterTools, entry).copy(idleNotified = false)

      // Deliver to direct participants
      val directs = for
        (name, ref) <- stateAfterMsg.participants
        if name != msg.sender
        if stateAfterMsg.directConversations.getOrElse(name, Set.empty).contains(conversation)
      yield name -> ref
      val stateAfterDirect = directs.foldLeft(stateAfterMsg) { case (s, (name, ref)) =>
        deliver(s, name, ref, Participant.ReceiveMessage(msg, conversation))
      }

      // Notify observers
      val events = List(
        Event(
          Event.ConversationMessage,
          Map(
            "conversation" -> conversation.asJson,
            "sender" -> msg.sender.asJson,
            "content" -> msg.content.asJson
          )
        )
      )
      val observers = for
        obs <- stateAfterDirect.observationsOf.getOrElse(conversation, Nil)
        if msg.sender != obs.participant
        ref <- stateAfterDirect.participants.get(obs.participant)
      yield obs.participant -> ref
      val stateAfterObservers = observers.foldLeft(stateAfterDirect) { case (s, (name, ref)) =>
        deliver(s, name, ref, Participant.ReceiveEvents(events))
      }

      // Feed to event sources
      val stateAfterSources = stateAfterObservers.eventSources.foldLeft(stateAfterObservers) {
        case (s, (name, ref)) => deliver(s, name, ref, EventSourceActor.SimulationUpdate(entry))
      }

      // Increment message count
      val count = stateAfterSources.messageCounts.getOrElse(conversation, 0) + 1
      stateAfterSources.copy(messageCounts =
        stateAfterSources.messageCounts + (conversation -> count)
      )

  /** Append an entry to the transcript and invoke the onEntry callback. */
  private def appendEntry(state: RunState, entry: TranscriptEntry): RunState =
    state.onEntry.foreach(_(entry))
    state.copy(transcript = state.transcript.append(entry))
