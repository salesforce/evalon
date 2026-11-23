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

import java.util.concurrent.{CompletableFuture, CompletionStage, LinkedBlockingQueue, TimeUnit}

import scala.concurrent.duration.*

import io.circe.Json
import io.circe.syntax.*
import org.apache.pekko.actor.testkit.typed.scaladsl.{ActorTestKit, FishingOutcomes, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

import com.salesforce.spearhead.evalon.llm.{Llm, LlmRequest, LlmResponse, LlmToolCall}
import com.salesforce.spearhead.evalon.model.*

class SimulatedParticipantTest extends AnyFunSuite with BeforeAndAfterAll:

  private val testKit = ActorTestKit()

  override def afterAll(): Unit = testKit.shutdownTestKit()

  /** One LLM call made by the participant, completed by the test via `reply`. */
  private case class LlmCall(request: LlmRequest):
    val result: CompletableFuture[LlmResponse] = CompletableFuture()
    def reply(calls: LlmToolCall*): Unit = result.complete(LlmResponse("", calls.toList))
    def replyText(text: String): Unit = result.complete(LlmResponse(text, Nil))
    def timeline: String = request.messages.map(_.content).mkString("\n")
    def offered: List[String] =
      request.tools
        .find(_.name == "send_message")
        .flatMap(
          _.inputSchema.hcursor
            .downField("properties")
            .downField("conversation")
            .get[List[String]]("enum")
            .toOption
        )
        .getOrElse(Nil)

  /** An LLM whose calls block until the test completes them. */
  private class ControlledLlm extends Llm:
    val calls = LinkedBlockingQueue[LlmCall]()
    def complete(request: LlmRequest): CompletionStage[LlmResponse] =
      val call = LlmCall(request)
      calls.put(call)
      call.result

    def nextCall(): LlmCall =
      val call = calls.poll(5, TimeUnit.SECONDS)
      assert(call != null, "expected another LLM call")
      call

    def assertNoMoreCalls(): Unit =
      assert(calls.poll(200, TimeUnit.MILLISECONDS) == null, "no further LLM calls expected")

  private def send(conversation: String, text: String) =
    LlmToolCall(
      "send_message",
      Json.obj("conversation" -> conversation.asJson, "text" -> text.asJson)
    )

  private val end = LlmToolCall("end_conversation", Json.obj())

  private val conversations = Map(
    "support_chat" -> ConversationConfig("support_chat", List("end_user", "rep")),
    "agent_assist" -> ConversationConfig("agent_assist", List("agent", "rep"))
  )

  /** The next response the rep sends the runner, skipping its idle reports. */
  private def expectResponse(runner: TestProbe[ScenarioRunner.Command]) =
    runner.fishForMessagePF(5.seconds) {
      case _: ScenarioRunner.Idle => FishingOutcomes.continueAndIgnore
      case _: ScenarioRunner.ParticipantResponse => FishingOutcomes.complete
    } match
      case List(r: ScenarioRunner.ParticipantResponse) => r
      case other => fail(s"expected one response, got $other")

  private def expectSent(
      runner: TestProbe[ScenarioRunner.Command],
      conversation: String,
      text: String
  ) =
    val response = expectResponse(runner)
    assert(response.conversation == conversation)
    assert(response.action == Action.Send(Message("rep", text)))

  /** Expect the rep to report idle with nothing sent before it. */
  private def expectIdle(runner: TestProbe[ScenarioRunner.Command]) =
    runner.fishForMessagePF(5.seconds) {
      case _: ScenarioRunner.Idle => FishingOutcomes.complete
      case other => FishingOutcomes.fail(s"expected idle, got $other")
    }

  private def spawnRep(
      llm: Llm,
      config: ParticipantConfig = ParticipantConfig("rep"),
      convs: Map[String, ConversationConfig] = conversations
  ) =
    val runner = testKit.createTestProbe[ScenarioRunner.Command]()
    val rep = testKit.spawn(SimulatedParticipant(config, llm, runner.ref, convs))
    expectIdle(runner) // the start report
    (rep, runner)

  test("one turn can answer the agent and ask the customer") {
    // The SRA stall: the agent asks for something only the customer can give. The rep must be
    // able to acknowledge the agent and ask the customer in the same turn.
    val llm = ControlledLlm()
    val (rep, runner) = spawnRep(llm)

    rep ! Participant.ReceiveMessage(
      Message("agent", "Please confirm their email."),
      "agent_assist"
    )
    val turn = llm.nextCall()
    assert(turn.offered.toSet == Set("support_chat", "agent_assist"))
    turn.reply(
      send("agent_assist", "Let me ask the customer for that."),
      send("support_chat", "Could you confirm the email on the order?")
    )
    expectSent(runner, "agent_assist", "Let me ask the customer for that.")
    expectSent(runner, "support_chat", "Could you confirm the email on the order?")
    expectIdle(runner)
    llm.assertNoMoreCalls()
  }

  test("the prompt shows every conversation in one timeline, with new lines marked") {
    val llm = ControlledLlm()
    val (rep, runner) = spawnRep(llm)

    rep ! Participant.ReceiveMessage(Message("end_user", "I want to cancel"), "support_chat")
    llm.nextCall().reply(send("support_chat", "Sure, one moment."))
    expectSent(runner, "support_chat", "Sure, one moment.")
    expectIdle(runner)

    rep ! Participant.ReceiveMessage(Message("agent", "Free cancellation applies."), "agent_assist")
    val timeline = llm.nextCall().timeline
    assert(timeline.contains("[end_user in support_chat]: I want to cancel"))
    assert(timeline.contains("[you in support_chat]: Sure, one moment."))
    assert(timeline.contains("(new) [agent in agent_assist]: Free cancellation applies."))
    assert(!timeline.contains("(new) [end_user"), "seen lines aren't marked new")
  }

  test("a message arriving during generation gets another turn that sees it") {
    val llm = ControlledLlm()
    val (rep, runner) = spawnRep(llm)

    rep ! Participant.ReceiveMessage(Message("end_user", "I want to cancel"), "support_chat")
    val first = llm.nextCall()
    rep ! Participant.ReceiveMessage(Message("agent", "Booking HTL-9012 found."), "agent_assist")
    first.reply(send("support_chat", "Sure, one moment."))
    expectSent(runner, "support_chat", "Sure, one moment.")

    val second = llm.nextCall()
    assert(second.timeline.contains("(new) [agent in agent_assist]: Booking HTL-9012 found."))
    // The first turn's reply precedes nothing it couldn't see: it's in the timeline as the rep's.
    assert(second.timeline.contains("[you in support_chat]: Sure, one moment."))
    second.reply()
    expectIdle(runner)
    llm.assertNoMoreCalls()
  }

  test("no tool call is silence, and the participant's own messages don't trigger a turn") {
    val llm = ControlledLlm()
    val (rep, runner) = spawnRep(llm)

    rep ! Participant.ReceiveMessage(Message("agent", "Nothing new."), "agent_assist")
    llm.nextCall().replyText("I'll stay quiet.") // text outside tool calls is not posted
    expectIdle(runner)

    rep ! Participant.ReceiveMessage(Message("end_user", "Hello?"), "support_chat")
    llm.nextCall().reply(send("support_chat", "Hi!"))
    expectSent(runner, "support_chat", "Hi!")
    expectIdle(runner)
    llm.assertNoMoreCalls()
  }

  test("end_conversation ends the run, in the conversation spoken in last") {
    val llm = ControlledLlm()
    val (rep, runner) = spawnRep(llm)

    rep ! Participant.ReceiveMessage(Message("end_user", "Thanks, bye!"), "support_chat")
    llm.nextCall().reply(send("support_chat", "Goodbye!"), end, send("support_chat", "dropped"))
    expectSent(runner, "support_chat", "Goodbye!")
    val ending = expectResponse(runner)
    assert(ending.conversation == "support_chat")
    assert(ending.action == Action.End)
    expectIdle(runner)
  }

  test("an invalid tool call is retried once with the problem, then sent") {
    val llm = ControlledLlm()
    val (rep, runner) = spawnRep(llm)

    rep ! Participant.ReceiveMessage(Message("end_user", "Hi"), "support_chat")
    llm.nextCall().reply(send("customer_chat", "Hello!"))
    val retry = llm.nextCall()
    assert(retry.timeline.contains("rejected and nothing was sent"))
    assert(retry.timeline.contains("customer_chat"))
    retry.reply(send("support_chat", "Hello!"))
    expectSent(runner, "support_chat", "Hello!")
  }

  test("an invalid tool call after the retry fails the run") {
    val llm = ControlledLlm()
    val (rep, runner) = spawnRep(llm)

    rep ! Participant.ReceiveMessage(Message("end_user", "Hi"), "support_chat")
    llm.nextCall().reply(LlmToolCall("wave", Json.obj()))
    llm.nextCall().reply(send("support_chat", "  "))
    val failed = runner.fishForMessagePF(5.seconds) {
      case _: ScenarioRunner.Idle => FishingOutcomes.continueAndIgnore
      case _: ScenarioRunner.ParticipantFailed => FishingOutcomes.complete
    }
    failed match
      case List(ScenarioRunner.ParticipantFailed("rep", cause)) =>
        assert(cause.getMessage.contains("non-empty text"))
      case other => fail(s"expected a failure, got $other")
  }

  test("a conversation isn't offered until its initiator has spoken") {
    val initiated = conversations.updated(
      "support_chat",
      ConversationConfig("support_chat", List("end_user", "rep"), initiatedBy = Some("end_user"))
    )
    val llm = ControlledLlm()
    val (rep, runner) = spawnRep(llm, convs = initiated)

    rep ! Participant.ReceiveMessage(Message("agent", "Ready when you are."), "agent_assist")
    val before = llm.nextCall()
    assert(before.offered == List("agent_assist"))
    assert(before.request.system.contains("not started yet"))
    before.reply()
    expectIdle(runner)

    rep ! Participant.ReceiveMessage(Message("end_user", "Hi"), "support_chat")
    assert(llm.nextCall().offered.toSet == Set("support_chat", "agent_assist"))
  }

  test("a templated participant still gets the harness instructions and tools") {
    val template = "You are Sam, a support rep. Follow the assistant's guidance."
    val llm = ControlledLlm()
    val (rep, _) = spawnRep(llm, ParticipantConfig("rep", template = Some(template)))

    rep ! Participant.ReceiveMessage(Message("agent", "Ask for their email."), "agent_assist")
    val prompt = llm.nextCall().request.system
    assert(prompt.startsWith(template))
    assert(prompt.contains("- \"agent_assist\" with: agent"))
    assert(prompt.contains("- \"support_chat\" with: end_user"))
    assert(prompt.contains("call send_message"), s"in:\n$prompt")
    assert(prompt.contains("end_conversation ONLY"))
  }

  private val repInitiates = conversations.updated(
    "support_chat",
    ConversationConfig("support_chat", List("end_user", "rep"), initiatedBy = Some("rep"))
  )

  private val kickoff = Message("system", "The conversation is starting. Please begin.")

  test("the kickoff turn offers only the conversation being opened") {
    val llm = ControlledLlm()
    val (rep, runner) = spawnRep(llm, convs = repInitiates)

    rep ! Participant.ReceiveMessage(kickoff, "support_chat")
    val opening = llm.nextCall()
    assert(opening.offered == List("support_chat"))
    opening.reply(send("support_chat", "Hi, how can I help?"))
    expectSent(runner, "support_chat", "Hi, how can I help?")
    expectIdle(runner)

    // Once opened, later turns offer every conversation again.
    rep ! Participant.ReceiveMessage(Message("agent", "I'm here to help."), "agent_assist")
    assert(llm.nextCall().offered.toSet == Set("support_chat", "agent_assist"))
  }

  test("a message that arrives with the kickoff gets its own turn after the opening") {
    // With a thinking delay, the agent's message joins the kickoff in one turn. That turn may only
    // open support_chat, so the agent's message must not be left as seen but unanswered.
    val llm = ControlledLlm()
    val fast = ParticipantConfig("rep", responseSpeed = Some(ResponseSpeed.Fast))
    val (rep, runner) = spawnRep(llm, fast, repInitiates)

    rep ! Participant.ReceiveMessage(kickoff, "support_chat")
    rep ! Participant.ReceiveMessage(Message("agent", "Ask for the order number."), "agent_assist")
    val opening = llm.nextCall()
    assert(opening.offered == List("support_chat"))
    opening.reply(send("support_chat", "Hi, how can I help?"))
    expectSent(runner, "support_chat", "Hi, how can I help?")

    val followUp = llm.calls.poll(6, java.util.concurrent.TimeUnit.SECONDS)
    assert(followUp != null, "expected a turn for the agent's message")
    assert(followUp.offered.toSet == Set("support_chat", "agent_assist"))
  }

  test("an invalid call after end_conversation doesn't cancel the ending") {
    // Calls after end_conversation are dropped unrun, so they mustn't be validated either.
    val llm = ControlledLlm()
    val (rep, runner) = spawnRep(llm)

    rep ! Participant.ReceiveMessage(Message("end_user", "Thanks, bye!"), "support_chat")
    llm.nextCall().reply(send("support_chat", "Goodbye!"), end, send("customer_chat", "oops"))
    expectSent(runner, "support_chat", "Goodbye!")
    assert(expectResponse(runner).action == Action.End)
    llm.assertNoMoreCalls()
  }
