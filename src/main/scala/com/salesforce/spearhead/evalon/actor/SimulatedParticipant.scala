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

import scala.concurrent.duration.*
import scala.util.{Failure, Random, Success}

import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors, TimerScheduler}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

import com.salesforce.spearhead.evalon.llm.{Llm, ChatMessage}
import com.salesforce.spearhead.evalon.model.{
  Action,
  ConversationConfig,
  Message,
  ParticipantConfig,
  ResponseSpeed,
  Signals
}

/**
 * An LLM-driven participant that plays a role in the simulation.
 *
 * Three states: idle → thinking → generating.
 *
 * Thinking simulates the participant absorbing incoming messages. If a new message arrives during
 * thinking, the timer resets (the participant waits to see if more is coming). Once the timer
 * fires, the participant enters generating (LLM call in flight). Messages arriving during
 * generation are queued. After generation, the participant always re-enters thinking before the
 * next LLM call, giving the full delay again.
 */
object SimulatedParticipant:

  private case class Setup(
      config: ParticipantConfig,
      llm: Llm,
      runner: ActorRef[ScenarioRunner.Command],
      conversations: Map[String, ConversationConfig]
  )

  // Internal command (LLM results come back via pipeToSelf)
  private[actor] case class LlmResult(conversation: String, kind: ResponseKind)

  // Internal command (timer fires after thinking delay)
  private[actor] case object ThinkingComplete

  private[actor] enum ResponseKind:
    case Text(content: String)
    case TextThenEnd(content: String) // final message before ending
    case End
    case Silent // nothing to say (e.g. [NO_QUESTION])

  /**
   * Per-cycle metadata. A cycle begins when idle receives a message and ends when the actor returns
   * to idle. Pending tracks reactive responses owed to convs that received input during an
   * in-flight generation; proactiveAttempted prevents re-visiting the same conv proactively;
   * historyAtCycleStart anchors the cross-conv activity check that drives proactive follow-ups.
   */
  private case class CycleState(
      pending: Set[String],
      proactiveAttempted: Set[String],
      historyAtCycleStart: Map[String, List[ChatMessage]]
  )

  private def buildSystemPrompt(
      config: ParticipantConfig,
      conversation: String,
      conversations: Map[String, ConversationConfig],
      history: Map[String, List[ChatMessage]]
  ): String =
    val conv = conversations.get(conversation)
    val others = conv.map(_.between.filter(_ != config.name).mkString(", ")).getOrElse("unknown")

    val contextSection = history.collect {
      case (convName, convHistory) if convName != conversation && convHistory.nonEmpty =>
        val otherParticipants = conversations
          .get(convName)
          .map(_.between.filter(_ != config.name).mkString(", "))
          .getOrElse("unknown")
        val lines = convHistory.map(_.content).mkString("\n")
        s"""\n\nYou also have an ongoing conversation in "$convName" with $otherParticipants. Recent messages there:\n$lines"""
    }.mkString

    s"""You are role-playing as a simulated participant.

Your name/role: ${config.name}
Your persona: ${config.persona}
Your goal: ${config.goal}

You are currently responding in the "$conversation" conversation with: $others
Address your response to them. Do NOT address participants from other conversations here.
Stay in character. Respond naturally based on the conversation so far.

Use ${Signals.End} ONLY when the conversation is genuinely complete: your goal has been achieved or the other party has clearly wrapped up. Never include ${Signals.End} while you are mid-task, waiting for information, or correcting yourself. When in doubt, do not end.

Respond with only your message content. Your output is delivered to the other party as-is.$contextSection"""

  type Cmd = Participant.Command | LlmResult | ThinkingComplete.type

  private val ThinkingTimerKey = "thinking"

  def apply(
      config: ParticipantConfig,
      llm: Llm,
      runner: ActorRef[ScenarioRunner.Command],
      conversations: Map[String, ConversationConfig] = Map.empty
  ): Behavior[Cmd] =
    val setup = Setup(config, llm, runner, conversations)
    Behaviors.withTimers(timers => idle(setup, Map.empty, timers))

  private def thinkingDelay(setup: Setup): FiniteDuration =
    setup.config.responseSpeed match
      case Some(ResponseSpeed.Fast)   => (2000 + Random.nextInt(1000)).millis
      case Some(ResponseSpeed.Medium) => (5000 + Random.nextInt(1000)).millis
      case Some(ResponseSpeed.Slow)   => (8000 + Random.nextInt(2000)).millis
      case None                       => 0.millis

  /** Idle state — waiting for messages. */
  private def idle(
      setup: Setup,
      history: Map[String, List[ChatMessage]],
      timers: TimerScheduler[Cmd]
  ): Behavior[Cmd] = Behaviors.receive {
    case (ctx, Participant.ReceiveMessage(msg, conversation)) =>
      val newHistory = appendToHistory(setup.config, history, msg, conversation)
      val cycle = CycleState(
        pending = Set.empty,
        proactiveAttempted = Set.empty,
        historyAtCycleStart = newHistory
      )
      enterThinking(setup, newHistory, conversation, cycle, timers, ctx)

    case (_, Participant.ReceiveEvents(_)) =>
      // Gap: simulated participants currently ignore observed events. Adding event handling
      // will require deciding how events feed into prompt context and what should trigger a
      // generation. Deferred until a concrete scenario needs it.
      Behaviors.same

    case (ctx, result: LlmResult) =>
      ctx.log.error("Unexpected LlmResult in idle state: {}", result)
      Behaviors.same

    case (_, ThinkingComplete) =>
      Behaviors.same
  }

  /** Enter thinking: schedule the delay (or skip straight to generating if no delay configured). */
  private def enterThinking(
      setup: Setup,
      history: Map[String, List[ChatMessage]],
      conversation: String,
      cycle: CycleState,
      timers: TimerScheduler[Cmd],
      ctx: ActorContext[Cmd]
  ): Behavior[Cmd] =
    val delay = thinkingDelay(setup)
    if delay > 0.millis then
      ctx.log.info(
        "{} entering thinking for {} ({}ms)",
        setup.config.name,
        conversation,
        delay.toMillis
      )
      timers.startSingleTimer(ThinkingTimerKey, ThinkingComplete, delay)
      thinking(setup, history, conversation, cycle, timers)
    else startGeneration(setup, history, conversation, cycle, timers, ctx)

  /** Thinking state — absorbing messages, timer running. Resets on any new input. */
  private def thinking(
      setup: Setup,
      history: Map[String, List[ChatMessage]],
      activeConversation: String,
      cycle: CycleState,
      timers: TimerScheduler[Cmd]
  ): Behavior[Cmd] = Behaviors.receive {

    case (ctx, Participant.ReceiveMessage(msg, conversation)) =>
      val newHistory = appendToHistory(setup.config, history, msg, conversation)

      val newCycle =
        if conversation == activeConversation then cycle
        else cycle.copy(pending = cycle.pending + conversation)

      val delay = thinkingDelay(setup)
      ctx.log.info(
        "{} resetting thinking timer (incoming in {}, {}ms)",
        setup.config.name,
        conversation,
        delay.toMillis
      )

      timers.startSingleTimer(ThinkingTimerKey, ThinkingComplete, delay)
      thinking(setup, newHistory, activeConversation, newCycle, timers)

    case (ctx, ThinkingComplete) =>
      ctx.log.info(
        "{} thinking complete, starting generation for {}",
        setup.config.name,
        activeConversation
      )
      startGeneration(setup, history, activeConversation, cycle, timers, ctx)

    case (_, Participant.ReceiveEvents(_)) =>
      // Gap: see idle's ReceiveEvents handler.
      Behaviors.same

    case (ctx, result: LlmResult) =>
      ctx.log.error("Unexpected LlmResult in thinking state: {}", result)
      Behaviors.same
  }

  /** Generating state — LLM call in flight. Accumulate incoming messages. */
  private def generating(
      setup: Setup,
      history: Map[String, List[ChatMessage]],
      activeConversation: String,
      cycle: CycleState,
      timers: TimerScheduler[Cmd]
  ): Behavior[Cmd] = Behaviors.receive {
    case (_, Participant.ReceiveMessage(msg, conversation)) =>
      val newHistory = appendToHistory(setup.config, history, msg, conversation)
      val newPending =
        if conversation == activeConversation then cycle.pending
        else cycle.pending + conversation
      generating(setup, newHistory, activeConversation, cycle.copy(pending = newPending), timers)

    case (_, Participant.ReceiveEvents(_)) =>
      // Gap: see idle's ReceiveEvents handler.
      Behaviors.same

    case (_, ThinkingComplete) =>
      Behaviors.same

    case (ctx, LlmResult(conversation, kind)) =>
      // Notifies the runner of a text message and appends it to local history.
      // Shared by Text and TextThenEnd, which differ only in what follows.
      def sendText(text: String): Map[String, List[ChatMessage]] =
        val msgs = history.getOrElse(conversation, Nil)
        setup.runner ! ScenarioRunner.ParticipantResponse(
          setup.config.name,
          conversation,
          Action.Send(Message(setup.config.name, text))
        )
        history + (conversation -> (msgs :+ ChatMessage.assistant(text)))

      val updatedCycle =
        cycle.copy(proactiveAttempted = cycle.proactiveAttempted + conversation)

      kind match
        case ResponseKind.Text(text) =>
          val newHistory = sendText(text)
          processPostGeneration(setup, newHistory, updatedCycle, timers, ctx)

        case ResponseKind.TextThenEnd(text) =>
          val newHistory = sendText(text)
          setup.runner ! ScenarioRunner.ParticipantResponse(
            setup.config.name,
            conversation,
            Action.End
          )
          idle(setup, newHistory, timers)

        case ResponseKind.End =>
          setup.runner ! ScenarioRunner.ParticipantResponse(
            setup.config.name,
            conversation,
            Action.End
          )
          idle(setup, history, timers)

        case ResponseKind.Silent =>
          ctx.log.debug("{} responded [NO_QUESTION] in {}", setup.config.name, conversation)
          processPostGeneration(setup, history, updatedCycle, timers, ctx)
  }

  private def startGeneration(
      setup: Setup,
      history: Map[String, List[ChatMessage]],
      conversation: String,
      cycle: CycleState,
      timers: TimerScheduler[Cmd],
      ctx: ActorContext[Cmd]
  ): Behavior[Cmd] =
    val systemPrompt =
      buildSystemPrompt(setup.config, conversation, setup.conversations, history)

    val msgs = history.get(conversation) match
      case Some(base) =>
        if base.lastOption.exists(_.role == "assistant") then
          base :+ ChatMessage.user("[system]: It's your turn to respond.")
        else base
      case None =>
        List(ChatMessage.user("[system]: The conversation is starting. Please begin."))

    ctx.pipeToSelf(setup.llm.completeAsync(systemPrompt, msgs)) {
      case Success(response) =>
        val text = Option(response).map(_.trim).filter(_.nonEmpty)
        // Only treat [END] as a termination signal when it's the trailing token, not when
        // it appears mid-text (which would happen e.g. if the model quoted it).
        val kind = text match
          case Some(t) if t.endsWith(Signals.End) =>
            val remaining = t.stripSuffix(Signals.End).trim
            if remaining.nonEmpty then ResponseKind.TextThenEnd(remaining)
            else ResponseKind.End
          case Some(t) if t == Signals.NoQuestion => ResponseKind.Silent
          case Some(t) if t.endsWith(Signals.NoQuestion) =>
            val remaining = t.stripSuffix(Signals.NoQuestion).trim
            if remaining.nonEmpty then ResponseKind.Text(remaining)
            else ResponseKind.Silent
          case Some(t) => ResponseKind.Text(t)
          case None => ResponseKind.Silent
        LlmResult(conversation, kind)
      case Failure(e) =>
        ctx.log.error("LLM call failed for participant {}", setup.config.name, e)
        LlmResult(conversation, ResponseKind.Silent)
    }

    generating(setup, history, conversation, cycle, timers)

  /**
   * After a generation completes, decide what (if anything) to do next within the same cycle.
   * Always re-enters thinking (full delay) before the next LLM call.
   *
   *   1. Reactive drain: pop one conv from `pending` and think before responding there.
   *   2. Proactive follow-up: when nothing is pending, look for a conv we spoke in earlier this
   *      cycle, where something happened in another conv since the cycle started (so we may have
   *      new context worth sharing). Each conv is attempted at most once per cycle; the LLM can
   *      decline by responding [NO_QUESTION].
   *   3. Otherwise, return to idle.
   */
  private def processPostGeneration(
      setup: Setup,
      history: Map[String, List[ChatMessage]],
      cycle: CycleState,
      timers: TimerScheduler[Cmd],
      ctx: ActorContext[Cmd]
  ): Behavior[Cmd] =
    cycle.pending.toList match
      case head :: tail =>
        val updatedCycle = cycle.copy(
          pending = tail.toSet,
          proactiveAttempted = cycle.proactiveAttempted + head
        )
        enterThinking(setup, history, head, updatedCycle, timers, ctx)

      case Nil =>
        // Check if any conversation grew during this cycle — if so, we may have new
        // cross-conversation context worth proactively relaying to other conversations.
        val crossConvActivity = history.exists { (conv, msgs) =>
          msgs.size > cycle.historyAtCycleStart.getOrElse(conv, Nil).size
        }
        val proactive =
          if !crossConvActivity then None
          else
            // Look for a conv where we last spoke and haven't already visited this cycle.
            // The just-completed conv is in proactiveAttempted, so we won't loop on it.
            history.collectFirst {
              case (conv, msgs)
                  if !cycle.proactiveAttempted.contains(conv)
                    && msgs.nonEmpty
                    && msgs.lastOption.exists(_.role == "assistant") =>
                conv
            }

        proactive match
          case Some(conv) =>
            val updatedCycle = cycle.copy(proactiveAttempted = cycle.proactiveAttempted + conv)
            enterThinking(setup, history, conv, updatedCycle, timers, ctx)
          case None =>
            idle(setup, history, timers)

  private def appendToHistory(
      config: ParticipantConfig,
      history: Map[String, List[ChatMessage]],
      msg: Message,
      conversation: String
  ): Map[String, List[ChatMessage]] =
    val msgs = history.getOrElse(conversation, Nil)
    val role = if msg.sender == config.name then "assistant" else "user"
    val content =
      if role == "assistant" then msg.content
      else s"[${msg.sender}]: ${msg.content}"

    // Merge consecutive messages with the same role
    val updatedMsgs = msgs.lastOption match
      case Some(last) if last.role == role =>
        msgs.init :+ ChatMessage(role, s"${last.content}\n$content")
      case _ =>
        msgs :+ ChatMessage(role, content)

    history + (conversation -> updatedMsgs)
