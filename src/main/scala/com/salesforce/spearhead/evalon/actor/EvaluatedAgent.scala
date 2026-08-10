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

import scala.util.{Failure, Success}

import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

import com.salesforce.spearhead.evalon.agent.Agent
import com.salesforce.spearhead.evalon.model.{Action, Event, HistoryEntry}

/** Wraps the Agent trait as a Pekko actor, bridging actor messages to Future-based calls.
  *
  * Owns the conversation history (including the agent's own outgoing messages and tool work) and
  * the pending event buffer; passes the slice plus a target conversation to the agent on every
  * step. Self-driven: when input arrives and the actor is idle, it starts a step. Inputs received
  * during an in-flight step are accumulated and processed afterward.
  */
object EvaluatedAgent:

  case class AgentResult private[EvaluatedAgent] (action: Action, conversation: String)

  /** Pending work accumulated while a step is in flight.
    *
    * Conversations that need a reply are tracked as a set (de-duplicated). Events are accumulated
    * as a flat list — original batching is incidental and not preserved.
    */
  private case class Pending(conversations: Set[String], events: List[Event])

  def apply(
      agent: Agent,
      runner: ActorRef[ScenarioRunner.Command],
      agentName: String,
      directConversations: Set[String]
  ): Behavior[Participant.Command | AgentResult] =
    idle(agent, runner, agentName, directConversations, Nil)

  private def idle(
      agent: Agent,
      runner: ActorRef[ScenarioRunner.Command],
      agentName: String,
      directConversations: Set[String],
      history: List[HistoryEntry]
  ): Behavior[Participant.Command | AgentResult] = Behaviors.receive {
    case (ctx, Participant.ReceiveMessage(msg, conversation)) =>
      val newHistory = history :+ HistoryEntry.Turn(conversation, msg.sender, msg.content)
      startStep(
        agent,
        runner,
        agentName,
        directConversations,
        newHistory,
        events = Nil,
        respondIn = conversation,
        Pending(Set.empty, Nil),
        ctx
      )

    case (ctx, Participant.ReceiveEvents(events)) =>
      val (newHistory, systemEvents) = absorbEvents(history, events)
      startStep(
        agent,
        runner,
        agentName,
        directConversations,
        newHistory,
        events = systemEvents,
        respondIn = directConversations.head,
        Pending(Set.empty, Nil),
        ctx
      )

    case (_, _: AgentResult) => Behaviors.same // stale result after state transition
  }

  /** Split observed events: conversation_message events become history turns; other events stay
    * as system events. This keeps the agent's view of the world durable across steps.
    */
  private def absorbEvents(
      history: List[HistoryEntry],
      events: List[Event]
  ): (List[HistoryEntry], List[Event]) =
    val (convEvents, systemEvents) = events.partition(_.name == "conversation_message")
    val turns = convEvents.map { e =>
      val conv = e.data.get("conversation").flatMap(_.asString).getOrElse("unknown")
      val sender = e.data.get("sender").flatMap(_.asString).getOrElse("unknown")
      val content = e.data.get("content").flatMap(_.asString).getOrElse("")
      HistoryEntry.Turn(conv, sender, content)
    }
    (history ++ turns, systemEvents)

  /** Append the agent's own outgoing action to history so future steps can see what the agent
    * has already done. Empty messages are skipped (consistent with the runner's filter).
    */
  private def recordAction(
      history: List[HistoryEntry],
      agentName: String,
      conversation: String,
      action: Action
  ): List[HistoryEntry] =
    action match
      case Action.End => history
      case Action.Send(message, toolTrace) =>
        val toolEntries = toolTrace.map(HistoryEntry.ToolUse(conversation, _))
        val msgEntry =
          if message.content.isEmpty then None
          else Some(HistoryEntry.Turn(conversation, agentName, message.content))
        history ++ toolEntries ++ msgEntry

  private def generating(
      agent: Agent,
      runner: ActorRef[ScenarioRunner.Command],
      agentName: String,
      directConversations: Set[String],
      history: List[HistoryEntry],
      pending: Pending
  ): Behavior[Participant.Command | AgentResult] = Behaviors.receive {
    case (_, Participant.ReceiveMessage(msg, conversation)) =>
      val newHistory = history :+ HistoryEntry.Turn(conversation, msg.sender, msg.content)
      val newPending = pending.copy(conversations = pending.conversations + conversation)
      generating(agent, runner, agentName, directConversations, newHistory, newPending)

    case (_, Participant.ReceiveEvents(events)) =>
      val (newHistory, systemEvents) = absorbEvents(history, events)
      val newPending = pending.copy(events = pending.events ++ systemEvents)
      generating(agent, runner, agentName, directConversations, newHistory, newPending)

    case (ctx, AgentResult(action, conversation)) =>
      runner ! ScenarioRunner.ParticipantResponse(agentName, conversation, action)
      val newHistory = recordAction(history, agentName, conversation, action)

      pending.conversations.headOption match
        case Some(conv) =>
          startStep(
            agent,
            runner,
            agentName,
            directConversations,
            newHistory,
            events = pending.events,
            respondIn = conv,
            Pending(pending.conversations - conv, Nil),
            ctx
          )
        case None if pending.events.nonEmpty =>
          startStep(
            agent,
            runner,
            agentName,
            directConversations,
            newHistory,
            events = pending.events,
            respondIn = directConversations.head,
            Pending(Set.empty, Nil),
            ctx
          )
        case None =>
          idle(agent, runner, agentName, directConversations, newHistory)
  }

  private def startStep(
      agent: Agent,
      runner: ActorRef[ScenarioRunner.Command],
      agentName: String,
      directConversations: Set[String],
      history: List[HistoryEntry],
      events: List[Event],
      respondIn: String,
      pending: Pending,
      ctx: ActorContext[Participant.Command | AgentResult]
  ): Behavior[Participant.Command | AgentResult] =
    ctx.pipeToSelf(agent.step(history, events, respondIn)) {
      case Success(action) => AgentResult(action, respondIn)
      case Failure(e) =>
        ctx.log.error("Agent step failed", e)
        AgentResult(Action.send(agentName, ""), respondIn)
    }
    generating(agent, runner, agentName, directConversations, history, pending)
