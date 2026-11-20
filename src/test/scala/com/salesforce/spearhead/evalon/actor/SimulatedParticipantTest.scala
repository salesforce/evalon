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

import org.apache.pekko.actor.testkit.typed.scaladsl.{ActorTestKit, FishingOutcomes, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

import com.salesforce.spearhead.evalon.llm.{ChatMessage, Llm}
import com.salesforce.spearhead.evalon.model.*

class SimulatedParticipantTest extends AnyFunSuite with BeforeAndAfterAll:

  private val testKit = ActorTestKit()

  override def afterAll(): Unit = testKit.shutdownTestKit()

  /** One LLM call made by the participant, completed by the test via `reply`. */
  private case class LlmCall(system: String, messages: List[ChatMessage]):
    val result: CompletableFuture[String] = CompletableFuture()
    def reply(text: String): Unit = result.complete(text)

  /** An LLM whose calls block until the test completes them. */
  private class ControlledLlm extends Llm:
    val calls = LinkedBlockingQueue[LlmCall]()
    def completeChat(system: String, messages: List[ChatMessage]): CompletionStage[String] =
      val call = LlmCall(system, messages)
      calls.put(call)
      call.result

    def nextCall(): LlmCall =
      val call = calls.poll(5, TimeUnit.SECONDS)
      assert(call != null, "expected another LLM call")
      call

    def assertNoMoreCalls(): Unit =
      assert(calls.poll(200, TimeUnit.MILLISECONDS) == null, "no further LLM calls expected")

  private val conversations = Map(
    "support_chat" -> ConversationConfig("support_chat", List("end_user", "rep")),
    "agent_assist" -> ConversationConfig("agent_assist", List("agent", "rep"))
  )

  /** The next response the rep sends the runner, skipping its idle reports. */
  private def expectResponse(runner: TestProbe[ScenarioRunner.Command]) =
    runner.fishForMessagePF(5.seconds) {
      case _: ScenarioRunner.Idle => FishingOutcomes.continueAndIgnore
      case _: ScenarioRunner.ParticipantResponse => FishingOutcomes.complete
    }

  private def spawnRep(llm: Llm, config: ParticipantConfig = ParticipantConfig("rep")) =
    val runner = testKit.createTestProbe[ScenarioRunner.Command]()
    val rep = testKit.spawn(SimulatedParticipant(config, llm, runner.ref, conversations))
    (rep, runner)

  test("a message arriving in the conversation being generated for is answered afterward") {
    val llm = ControlledLlm()
    val (rep, runner) = spawnRep(llm)

    rep ! Participant.ReceiveMessage(Message("end_user", "I want to cancel"), "support_chat")
    val first = llm.nextCall()

    // Arrives while the reply to the first message is being generated.
    rep ! Participant.ReceiveMessage(
      Message("end_user", "Can I get a full refund?"),
      "support_chat"
    )
    first.reply("Sure, one moment.")
    expectResponse(runner)

    val second = llm.nextCall()
    // History stays chronological (matching the transcript): the new message precedes the reply
    // that was generated without it, and the participant is nudged to respond again.
    val contents = second.messages.map(_.content)
    val refundAt = contents.indexWhere(_.contains("full refund"))
    assert(refundAt >= 0)
    assert(refundAt < contents.indexOf("Sure, one moment."))
    assert(contents.last.contains("your turn to respond"))

    second.reply("Yes, you're eligible for a full refund.")
    expectResponse(runner)
    llm.assertNoMoreCalls()
  }

  test("a customer message arriving during a proactive follow-up after agent_assist is answered") {
    val llm = ControlledLlm()
    val (rep, runner) = spawnRep(llm)

    // Cycle 1: rep answers the customer, then goes idle.
    rep ! Participant.ReceiveMessage(Message("end_user", "I want to cancel"), "support_chat")
    llm.nextCall().reply("Sure, one moment.")
    expectResponse(runner)
    llm.assertNoMoreCalls()

    // Cycle 2: agent posts in agent_assist, rep replies there, then proactively revisits
    // support_chat. The customer writes while that follow-up is being generated.
    rep ! Participant.ReceiveMessage(Message("agent", "Free cancellation applies."), "agent_assist")
    llm.nextCall().reply("Thanks.")
    expectResponse(runner)

    val followUp = llm.nextCall()
    rep ! Participant.ReceiveMessage(
      Message("end_user", "Can I get a full refund?"),
      "support_chat"
    )
    followUp.reply("Good news, cancellation is free.")
    expectResponse(runner)

    val answer = llm.nextCall()
    assert(answer.messages.exists(_.content.contains("full refund")))
    answer.reply("Yes, the full $500 will be refunded.")
    expectResponse(runner)
    llm.assertNoMoreCalls()
  }

  test("a message in another conversation during generation is still answered") {
    val llm = ControlledLlm()
    val (rep, runner) = spawnRep(llm)

    rep ! Participant.ReceiveMessage(Message("end_user", "I want to cancel"), "support_chat")
    val first = llm.nextCall()
    rep ! Participant.ReceiveMessage(Message("agent", "Booking HTL-9012 found."), "agent_assist")
    first.reply("Sure, one moment.")
    expectResponse(runner)

    val second = llm.nextCall()
    assert(second.messages.last.content.contains("HTL-9012"))
    second.reply(Signals.NoQuestion)
    llm.assertNoMoreCalls()
  }

  /**
   * Drive the rep through a support_chat reply, then an agent_assist turn and its support_chat
   * follow-up, returning the system prompts of those last two turns.
   */
  private def assistAndFollowUpPrompts(config: ParticipantConfig): (String, String) =
    val llm = ControlledLlm()
    val (rep, runner) = spawnRep(llm, config)
    rep ! Participant.ReceiveMessage(Message("end_user", "I want to cancel"), "support_chat")
    llm.nextCall().reply("Sure, one moment.")
    expectResponse(runner)
    llm.assertNoMoreCalls()

    rep ! Participant.ReceiveMessage(Message("agent", "Ask for their email."), "agent_assist")
    val assist = llm.nextCall()
    assist.reply("Will do.")
    expectResponse(runner)
    val followUp = llm.nextCall()
    followUp.reply(Signals.NoQuestion)
    llm.assertNoMoreCalls()
    (assist.system, followUp.system)

  private val assistLine = "responding in the \"agent_assist\" conversation with: agent"
  private val supportLine = "responding in the \"support_chat\" conversation with: end_user"

  test("a templated participant is told which conversation it is responding in") {
    val template = "You are Sam, a support rep. Follow the assistant's guidance."
    val (assist, followUp) =
      assistAndFollowUpPrompts(ParticipantConfig("rep", template = Some(template)))

    for (prompt, line) <- List(assist -> assistLine, followUp -> supportLine) do
      assert(prompt.startsWith(template))
      assert(prompt.contains(line), s"missing routing line in:\n$prompt")
      assert(prompt.contains("Do NOT address participants from other conversations here."))
      assert(prompt.contains(Signals.End))
  }

  test("the default prompt names the conversation exactly once") {
    val (assist, followUp) = assistAndFollowUpPrompts(
      ParticipantConfig("rep", persona = "You are Sam.", goal = "Resolve the request.")
    )
    for (prompt, line) <- List(assist -> assistLine, followUp -> supportLine) do
      assert(prompt.startsWith("You are role-playing as a simulated participant."))
      assert(prompt.sliding(line.length).count(_ == line) == 1, s"in:\n$prompt")
  }
