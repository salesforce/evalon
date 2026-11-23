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
import scala.util.{Failure, Random, Success, Try}

import io.circe.Json
import io.circe.syntax.*
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors, TimerScheduler}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

import com.salesforce.spearhead.evalon.llm.{ChatMessage, Llm, LlmRequest, LlmResponse, LlmTool}
import com.salesforce.spearhead.evalon.model.{
  Action,
  ConversationConfig,
  Message,
  ParticipantConfig,
  ResponseSpeed
}

/**
 * An LLM-driven participant that plays a role in the simulation.
 *
 * It keeps one timeline of everything said in its conversations. Whenever something from another
 * participant is newer than the last prompt it built, it takes a turn: the model sees the whole
 * timeline, with new lines marked, and acts only through tool calls. It may send several messages,
 * to any of its conversations, in one turn, or call nothing and stay silent.
 *
 * Three states: idle → thinking → generating. Thinking simulates absorbing incoming messages: a new
 * message resets the `response_speed` delay. Messages arriving during generation are added to the
 * timeline; if any is from someone else, another turn follows, after the full delay again.
 */
object SimulatedParticipant:

  private case class Setup(
      config: ParticipantConfig,
      llm: Llm,
      runner: ActorRef[ScenarioRunner.Command],
      conversations: List[ConversationConfig],
      idleReporter: IdleReporter
  )

  /**
   * One line of the timeline. The participant's own lines have its name as `sender` and are seen
   * from the start; others' lines are seen once a prompt has shown them.
   */
  private case class Line(conversation: String, sender: String, content: String, seen: Boolean)

  /** The runner's kickoff message: it opens a conversation for its initiator. */
  private val KickoffSender = "system"

  private case class History(lines: Vector[Line]):
    def add(line: Line): History = copy(lines = lines :+ line)
    def markSeen(conversations: Set[String]): History =
      copy(lines = lines.map(l => if conversations(l.conversation) then l.copy(seen = true) else l))
    def hasUnseen: Boolean = lines.exists(!_.seen)

    /** Conversations whose kickoff the participant hasn't had a turn for yet. */
    def pendingKickoffs: List[String] =
      lines
        .collect { case l if !l.seen && l.sender == KickoffSender => l.conversation }
        .distinct
        .toList

  /** What one turn asked for, after validating the model's tool calls. */
  private[actor] enum TurnAction:
    case Send(conversation: String, text: String)
    case End

  // Internal command: an LLM call finished. Carries what's needed to validate and retry it.
  private[actor] case class TurnResult(
      request: LlmRequest,
      allowed: List[String],
      attempt: Int,
      result: Try[LlmResponse]
  )

  // Internal command (timer fires after thinking delay)
  private[actor] case object ThinkingComplete

  type Cmd = Participant.Command | TurnResult | ThinkingComplete.type

  private val ThinkingTimerKey = "thinking"

  private[actor] val SendMessage = "send_message"
  private[actor] val EndConversation = "end_conversation"

  /** Retries of a turn whose tool calls were invalid, before the run fails. */
  private val MaxRetries = 1

  /**
   * `conversations` is the scenario's conversations by name; the participant keeps those it is in.
   * It is required: a participant with none has nowhere to post and would stay silent forever.
   */
  def apply(
      config: ParticipantConfig,
      llm: Llm,
      runner: ActorRef[ScenarioRunner.Command],
      conversations: Map[String, ConversationConfig]
  ): Behavior[Cmd] =
    Behaviors.setup { _ =>
      val mine = conversations.values.filter(_.between.contains(config.name)).toList.sortBy(_.name)
      val setup = Setup(config, llm, runner, mine, IdleReporter(config.name, runner))
      setup.idleReporter.reportIdle() // waits for a message before it speaks
      Behaviors.withTimers(timers => idle(setup, History(Vector.empty), timers))
    }

  private def thinkingDelay(setup: Setup): FiniteDuration =
    setup.config.responseSpeed match
      case Some(ResponseSpeed.Fast) => (2000 + Random.nextInt(1000)).millis
      case Some(ResponseSpeed.Medium) => (5000 + Random.nextInt(1000)).millis
      case Some(ResponseSpeed.Slow) => (8000 + Random.nextInt(2000)).millis
      case None => 0.millis

  private def received(setup: Setup, history: History, msg: Message, conversation: String) =
    setup.idleReporter.received()
    history.add(Line(conversation, msg.sender, msg.content, seen = msg.sender == setup.config.name))

  /** Return to idle, telling the runner (for idle detection). */
  private def goIdle(setup: Setup, history: History, timers: TimerScheduler[Cmd]): Behavior[Cmd] =
    setup.idleReporter.reportIdle()
    idle(setup, history, timers)

  /** Idle state — waiting for messages. */
  private def idle(
      setup: Setup,
      history: History,
      timers: TimerScheduler[Cmd]
  ): Behavior[Cmd] = Behaviors.receive {
    case (ctx, Participant.ReceiveMessage(msg, conversation)) =>
      enterThinking(setup, received(setup, history, msg, conversation), timers, ctx)

    case (_, Participant.ReceiveEvents(_)) =>
      // Gap: simulated participants currently ignore observed events. Adding event handling
      // will require deciding how events feed into prompt context and what should trigger a
      // turn. Deferred until a concrete scenario needs it.
      // Still idle after handling a delivery, so say so: the runner counted it.
      setup.idleReporter.received()
      setup.idleReporter.reportIdle()
      Behaviors.same

    case (ctx, result: TurnResult) =>
      ctx.log.error("Unexpected TurnResult in idle state: {}", result)
      Behaviors.same

    case (_, ThinkingComplete) =>
      Behaviors.same
  }

  /** Enter thinking: schedule the delay (or skip straight to the turn if no delay configured). */
  private def enterThinking(
      setup: Setup,
      history: History,
      timers: TimerScheduler[Cmd],
      ctx: ActorContext[Cmd]
  ): Behavior[Cmd] =
    val delay = thinkingDelay(setup)
    if delay > 0.millis then
      ctx.log.info("{} entering thinking ({}ms)", setup.config.name, delay.toMillis)
      timers.startSingleTimer(ThinkingTimerKey, ThinkingComplete, delay)
      thinking(setup, history, timers)
    else startTurn(setup, history, timers, ctx)

  /** Thinking state — absorbing messages, timer running. Resets on any new input. */
  private def thinking(
      setup: Setup,
      history: History,
      timers: TimerScheduler[Cmd]
  ): Behavior[Cmd] = Behaviors.receive {
    case (ctx, Participant.ReceiveMessage(msg, conversation)) =>
      val delay = thinkingDelay(setup)
      ctx.log.info(
        "{} resetting thinking timer (incoming in {}, {}ms)",
        setup.config.name,
        conversation,
        delay.toMillis
      )
      timers.startSingleTimer(ThinkingTimerKey, ThinkingComplete, delay)
      thinking(setup, received(setup, history, msg, conversation), timers)

    case (ctx, ThinkingComplete) =>
      ctx.log.info("{} thinking complete, starting turn", setup.config.name)
      startTurn(setup, history, timers, ctx)

    case (_, Participant.ReceiveEvents(_)) =>
      // Gap: see idle's ReceiveEvents handler.
      setup.idleReporter.received()
      Behaviors.same

    case (ctx, result: TurnResult) =>
      ctx.log.error("Unexpected TurnResult in thinking state: {}", result)
      Behaviors.same
  }

  /**
   * Conversations the participant may post in now: those with no initiator, those it initiates, and
   * those whose initiator has spoken.
   */
  private def allowedConversations(setup: Setup, history: History): List[String] =
    setup.conversations.collect {
      case conv
          if conv.initiatedBy.forall(initiator =>
            initiator == setup.config.name || history.lines
              .exists(line => line.conversation == conv.name && line.sender == initiator)
          ) =>
        conv.name
    }

  private def startTurn(
      setup: Setup,
      history: History,
      timers: TimerScheduler[Cmd],
      ctx: ActorContext[Cmd]
  ): Behavior[Cmd] =
    // A kickoff turn only opens its conversation(s): offered anything else, the opening line could
    // go to the wrong one. It shows and marks seen only those conversations, so anything else that
    // arrived meanwhile is still new and gets the next turn.
    val opening = history.pendingKickoffs
    val shown = if opening.nonEmpty then opening.toSet else setup.conversations.map(_.name).toSet
    val allowed =
      if opening.nonEmpty then opening else allowedConversations(setup, history)
    if allowed.isEmpty then
      ctx.log.info("{} has no conversation it may post in yet", setup.config.name)
      goIdle(setup, history.markSeen(shown), timers)
    else
      val timeline = renderTimeline(setup.config.name, history, shown, opening)
      val request = LlmRequest(
        system = buildSystemPrompt(setup.config, setup.conversations, allowed.toSet),
        messages = List(ChatMessage.user(timeline)),
        tools = turnTools(allowed)
      )
      call(setup, request, allowed, attempt = 0, ctx)
      generating(setup, history.markSeen(shown), timers)

  private def call(
      setup: Setup,
      request: LlmRequest,
      allowed: List[String],
      attempt: Int,
      ctx: ActorContext[Cmd]
  ): Unit =
    ctx.pipeToSelf(setup.llm.completeAsync(request))(TurnResult(request, allowed, attempt, _))

  /** Generating state — LLM call in flight. Incoming messages join the timeline. */
  private def generating(
      setup: Setup,
      history: History,
      timers: TimerScheduler[Cmd]
  ): Behavior[Cmd] = Behaviors.receive {
    case (_, Participant.ReceiveMessage(msg, conversation)) =>
      generating(setup, received(setup, history, msg, conversation), timers)

    case (_, Participant.ReceiveEvents(_)) =>
      // Gap: see idle's ReceiveEvents handler.
      setup.idleReporter.received()
      Behaviors.same

    case (_, ThinkingComplete) =>
      Behaviors.same

    case (ctx, TurnResult(_, _, _, Failure(cause))) =>
      // A failed simulator LLM call is a broken harness, not a real "say nothing". Fail the
      // run fast instead of leaving this participant silently mute.
      ctx.log.error("LLM call failed for participant {}", setup.config.name, cause)
      fail(setup, cause)

    case (ctx, TurnResult(request, allowed, attempt, Success(response))) =>
      parseTurn(response, allowed) match
        case Left(problem) if attempt < MaxRetries =>
          ctx.log.warn("{} made invalid tool calls, retrying: {}", setup.config.name, problem)
          call(setup, withRetryNote(request, problem), allowed, attempt + 1, ctx)
          Behaviors.same
        case Left(problem) =>
          fail(setup, IllegalStateException(s"Invalid tool calls after a retry: $problem"))
        case Right(actions) =>
          val text = Option(response.text).map(_.trim).getOrElse("")
          if text.nonEmpty then
            ctx.log.debug("{} wrote text outside tool calls: {}", setup.config.name, text)
          if actions.isEmpty then ctx.log.info("{} stayed silent (no tool call)", setup.config.name)
          act(setup, history, actions, timers, ctx)
  }

  private def fail(setup: Setup, cause: Throwable): Behavior[Cmd] =
    setup.runner ! ScenarioRunner.ParticipantFailed(setup.config.name, cause)
    Behaviors.stopped

  /**
   * Send the turn's messages in order, then end the run if the turn ends with `end_conversation`
   * ([[parseTurn]] drops anything after it). Otherwise take another turn if something unseen
   * arrived meanwhile.
   */
  private def act(
      setup: Setup,
      history: History,
      actions: List[TurnAction],
      timers: TimerScheduler[Cmd],
      ctx: ActorContext[Cmd]
  ): Behavior[Cmd] =
    val name = setup.config.name
    actions match
      case TurnAction.Send(conversation, text) :: rest =>
        setup.runner ! ScenarioRunner.ParticipantResponse(
          name,
          conversation,
          Action.Send(Message(name, text))
        )
        act(setup, history.add(Line(conversation, name, text, seen = true)), rest, timers, ctx)

      case TurnAction.End :: _ =>
        // The runner records [END] in a conversation: the one spoken in last.
        val conversation = history.lines.reverseIterator
          .collectFirst { case line if line.sender != KickoffSender => line.conversation }
          .orElse(history.lines.lastOption.map(_.conversation))
          .getOrElse(setup.conversations.head.name)
        setup.runner ! ScenarioRunner.ParticipantResponse(name, conversation, Action.End)
        goIdle(setup, history, timers)

      case Nil =>
        if history.hasUnseen then enterThinking(setup, history, timers, ctx)
        else goIdle(setup, history, timers)

  /** The turn's actions, or why its tool calls are invalid. */
  private[actor] def parseTurn(
      response: LlmResponse,
      allowed: List[String]
  ): Either[String, List[TurnAction]] =
    // Calls after end_conversation never run (the run ends), so they aren't validated: an invalid
    // one must not cancel a valid ending.
    val all = Option(response.toolCalls).getOrElse(Nil)
    val endAt = all.indexWhere(_.name == EndConversation)
    val calls = if endAt >= 0 then all.take(endAt + 1) else all
    val parsed = calls.map { call =>
      val args = Option(call.arguments).getOrElse(Json.Null).hcursor
      call.name match
        case SendMessage =>
          (args.get[String]("conversation").toOption, args.get[String]("text").toOption) match
            case (Some(conv), _) if !allowed.contains(conv) =>
              Left(
                s"""$SendMessage to "$conv", which is not one of ${allowed.mkString(", ")}"""
              )
            case (Some(conv), Some(text)) if text.trim.nonEmpty =>
              Right(TurnAction.Send(conv, text.trim))
            case _ =>
              Left(s"$SendMessage needs a conversation and non-empty text, got ${call.arguments}")
        case EndConversation => Right(TurnAction.End)
        case other => Left(s"unknown tool $other")
    }
    val problems = parsed.collect { case Left(problem) => problem }
    if problems.nonEmpty then Left(problems.mkString("; "))
    else Right(parsed.collect { case Right(action) => action })

  private def withRetryNote(request: LlmRequest, problem: String): LlmRequest =
    val note =
      s"\n\n[system]: Your last reply was rejected and nothing was sent: $problem. Try again."
    request.copy(messages =
      request.messages.init :+ ChatMessage.user(request.messages.last.content + note)
    )

  private def turnTools(allowed: List[String]): List[LlmTool] =
    List(
      LlmTool(
        SendMessage,
        "Send a message in one of your conversations. Only that conversation's participants " +
          "see it. Call it once per message; you may send to several conversations in one turn.",
        Json.obj(
          "type" -> "object".asJson,
          "properties" -> Json.obj(
            "conversation" -> Json.obj(
              "type" -> "string".asJson,
              "enum" -> allowed.asJson,
              "description" -> "The conversation to post in.".asJson
            ),
            "text" -> Json.obj(
              "type" -> "string".asJson,
              "description" -> "The message, exactly as its readers should see it.".asJson
            )
          ),
          "required" -> List("conversation", "text").asJson
        )
      ),
      LlmTool(
        EndConversation,
        "End the whole simulation. Only when your goal is achieved or the others have clearly " +
          "wrapped up.",
        Json.obj("type" -> "object".asJson, "properties" -> Json.obj())
      )
    )

  /**
   * The timeline of the `shown` conversations as one transcript, lines tagged with sender and
   * conversation. On a kickoff turn (`opening` non-empty) the nudge asks to open those.
   */
  private def renderTimeline(
      name: String,
      history: History,
      shown: Set[String],
      opening: List[String]
  ): String =
    val lines = history.lines.collect {
      case line if shown(line.conversation) =>
        val who = if line.sender == name then "you" else line.sender
        val marker = if line.seen then "" else "(new) "
        s"$marker[$who in ${line.conversation}]: ${line.content}"
    }
    val nudge =
      if opening.nonEmpty then
        val names = opening.map(c => s"\"$c\"").mkString(", ")
        s"[system]: Open $names now: call $SendMessage there with your opening message."
      else
        s"[system]: It's your turn. Call $SendMessage for each message you want to send, " +
          "or call no tool to stay silent."
    (lines :+ "" :+ nudge).mkString("\n")

  private def buildSystemPrompt(
      config: ParticipantConfig,
      conversations: List[ConversationConfig],
      allowed: Set[String]
  ): String =
    val conversationList = conversations.map { conv =>
      val others = conv.between.filter(_ != config.name).mkString(", ")
      val notStarted = conv.initiatedBy match
        case Some(initiator) if !allowed.contains(conv.name) =>
          s" (not started yet: you can post here once $initiator has)"
        case _ => ""
      s"""- "${conv.name}" with: $others$notStarted"""
    }

    // How the harness works: included with or without a template, like the [END] rule used to be.
    val harness =
      s"""You are in these conversations:
${conversationList.mkString("\n")}

You see all of them in one timeline, in order. Each line is tagged [sender in conversation]; your own lines are tagged [you in conversation], and lines marked (new) arrived since your last turn.

To send a message, call $SendMessage with the conversation and the text. A message reaches only the participants of that conversation, so address them, not participants of your other conversations. You may call $SendMessage several times in one turn, for example to answer in one conversation and ask something in another. If nothing needs saying, call no tool: you stay silent, and get another turn when someone writes to you. Text outside tool calls is never delivered.

Call $EndConversation ONLY when the conversation is genuinely complete: your goal has been achieved or the other party has clearly wrapped up. Never end while you are mid-task, waiting for information, or correcting yourself. When in doubt, do not end."""

    config.template.map(_.trim).filter(_.nonEmpty) match
      case Some(template) => s"$template\n\n$harness"
      case None =>
        s"""You are role-playing as a simulated participant.

Your name/role: ${config.name}
Your persona: ${config.persona}
Your goal: ${config.goal}

Stay in character. Respond naturally based on the conversation so far.

$harness"""
