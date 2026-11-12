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

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

import scala.concurrent.{Future, Promise}

import io.circe.syntax.*
import org.apache.pekko.actor.testkit.typed.scaladsl.ActorTestKit
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

import com.salesforce.spearhead.evalon.agent.Agent
import com.salesforce.spearhead.evalon.model.*

class EvaluatedAgentTest extends AnyFunSuite with BeforeAndAfterAll:

  private val testKit = ActorTestKit()

  override def afterAll(): Unit = testKit.shutdownTestKit()

  /** One agent step invocation, completed by the test via `reply`. */
  private case class StepCall(history: List[HistoryEntry], respondIn: String):
    val reply: Promise[Action] = Promise()

  /** An agent whose steps block until the test completes them. */
  private class ControlledAgent extends Agent:
    val calls = LinkedBlockingQueue[StepCall]()
    def step(history: List[HistoryEntry], events: List[Event], respondIn: String): Future[Action] =
      val call = StepCall(history, respondIn)
      calls.put(call)
      call.reply.future

    def nextCall(): StepCall =
      val call = calls.poll(5, TimeUnit.SECONDS)
      assert(call != null, "expected another agent step")
      call

  private def observed(conversation: String, sender: String, content: String): Event =
    Event(
      "conversation_message",
      Map(
        "conversation" -> conversation.asJson,
        "sender" -> sender.asJson,
        "content" -> content.asJson
      )
    )

  test("an observed message arriving mid-step triggers another step") {
    val runner = testKit.createTestProbe[ScenarioRunner.Command]()
    val agent = ControlledAgent()
    val actor = testKit.spawn(EvaluatedAgent(agent, runner.ref, "agent", Set("assist")))

    actor ! Participant.ReceiveEvents(List(observed("chat", "end_user", "I want to cancel")))
    val first = agent.nextCall()

    // Arrives while the first step is still in flight.
    actor ! Participant.ReceiveEvents(List(observed("chat", "end_user", "Full refund please")))
    first.reply.success(Action.send("agent", "Look up the booking"))
    runner.expectMessageType[ScenarioRunner.ParticipantResponse]

    val second = agent.nextCall()
    assert(second.respondIn == "assist")
    assert(second.history.exists {
      case HistoryEntry.Turn("chat", "end_user", "Full refund please", _) => true
      case _ => false
    })

    second.reply.success(Action.send("agent", ""))
    runner.expectMessageType[ScenarioRunner.ParticipantResponse]
    assert(agent.calls.poll(200, TimeUnit.MILLISECONDS) == null, "no further steps expected")
  }

  test("several observed messages mid-step coalesce into a single follow-up step") {
    val runner = testKit.createTestProbe[ScenarioRunner.Command]()
    val agent = ControlledAgent()
    val actor = testKit.spawn(EvaluatedAgent(agent, runner.ref, "agent", Set("assist")))

    actor ! Participant.ReceiveEvents(List(observed("chat", "end_user", "hi")))
    val first = agent.nextCall()

    actor ! Participant.ReceiveEvents(List(observed("chat", "representative", "one moment")))
    actor ! Participant.ReceiveEvents(List(observed("chat", "end_user", "thanks")))
    first.reply.success(Action.send("agent", ""))
    runner.expectMessageType[ScenarioRunner.ParticipantResponse]

    val second = agent.nextCall()
    assert(second.history.count(_.isInstanceOf[HistoryEntry.Turn]) == 3)
    second.reply.success(Action.send("agent", ""))
    runner.expectMessageType[ScenarioRunner.ParticipantResponse]
    assert(agent.calls.poll(200, TimeUnit.MILLISECONDS) == null, "no further steps expected")
  }
