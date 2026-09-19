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

package com.salesforce.spearhead.evalon

import java.time.Duration
import java.util.concurrent.CompletableFuture

import scala.concurrent.Await
import scala.concurrent.duration.*

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

import com.salesforce.spearhead.evalon.agent.{AgentReply, SimpleAgent}
import com.salesforce.spearhead.evalon.llm.Llm
import com.salesforce.spearhead.evalon.model.*

class EvalonRunnerTest extends AnyFunSuite with BeforeAndAfterAll:

  private val system = EvalonRunner.newSystem("evalon-test")
  private val evalon = EvalonRunner(system)

  override def afterAll(): Unit =
    system.terminate()
    Await.ready(system.whenTerminated, 30.seconds)

  test("sequential runs reuse one caller-owned actor system") {
    val llm: Llm = prompt =>
      val text =
        if prompt.contains("expert evaluator") then
          """{"passed":true,"score":1.0,"reasoning":"ok"}"""
        else Signals.End
      CompletableFuture.completedFuture(text)

    val agent: SimpleAgent = (_, _, _) => CompletableFuture.completedFuture(AgentReply.end())
    val options = EvalonRunOptions()
      .withSimulationTimeout(Duration.ofSeconds(15))
      .withJudgeTimeout(Duration.ofSeconds(5))

    val first = evalon.runSimple(scenario("one"), agent, llm, options)
    val second = evalon.runSimple(scenario("two"), agent, llm, options)

    assert(first.getScenarioName == "one")
    assert(second.getScenarioName == "two")
    assert(first.getSummary == "ok")
    assert(second.getSummary == "ok")
  }

  test("AgentReply trace is attached to the send action") {
    import scala.concurrent.ExecutionContext.Implicits.global
    import io.circe.Json
    import io.circe.syntax.*

    val agent = SimpleAgent.toAgent(
      (_, _, _) =>
        CompletableFuture.completedFuture(AgentReply.send("hello", """{"intent":"greet"}""")),
      "agent"
    )
    Await.result(agent.step(Nil, Nil, "chat"), 5.seconds) match
      case Action.Send(msg, _) =>
        assert(msg.content == "hello")
        assert(msg.trace.contains(Json.obj("intent" -> "greet".asJson)))
      case other => fail(s"expected Send, got $other")
  }

  test("transcript JSON includes message trace") {
    import io.circe.Json
    import io.circe.syntax.*

    val transcript = Transcript().addMessage(
      sender = "agent",
      content = "hello",
      conversation = Some("chat"),
      trace = Some(Json.obj("intent" -> "greet".asJson)),
    )
    val entry = transcript.toJson.asArray.get.head
    assert(entry.hcursor.get[String]("content").toOption.contains("hello"))
    assert(entry.hcursor.downField("trace").get[String]("intent").toOption.contains("greet"))
  }

  test("history turns expose trace to SimpleAgent") {
    import scala.concurrent.ExecutionContext.Implicits.global
    import io.circe.Json

    var captured: String = null
    val simple: SimpleAgent = (_, history, _) =>
      captured = history.get(0).getTrace.orElse(null)
      CompletableFuture.completedFuture(AgentReply.end())
    val agent = SimpleAgent.toAgent(simple, "agent")
    val history = List(
      HistoryEntry.Turn("chat", "end_user", "hi", Some(Json.obj("k" -> Json.fromString("v"))))
    )
    Await.result(agent.step(history, Nil, "chat"), 5.seconds)
    assert(captured == """{"k":"v"}""")
  }

  test("invalid AgentReply trace fails the step") {
    import scala.concurrent.ExecutionContext.Implicits.global

    val agent = SimpleAgent.toAgent(
      (_, _, _) => CompletableFuture.completedFuture(AgentReply.send("hello", "not-json")),
      "agent"
    )
    intercept[IllegalArgumentException] {
      Await.result(agent.step(Nil, Nil, "chat"), 5.seconds)
    }
  }

  private def scenario(name: String): Scenario =
    Scenario(
      name = name,
      description = "shared-system test",
      participants = Map(
        "end_user" -> ParticipantConfig("end_user", ParticipantType.Simulated),
        "agent" -> ParticipantConfig("agent", ParticipantType.Evaluated)
      ),
      conversations = List(
        ConversationConfig("chat", List("end_user", "agent"), initiatedBy = Some("end_user"))
      ),
      evalCriteria = List(
        EvalCriterion(
          name = "ok",
          description = "ok",
          criterionType = CriterionType.Binary,
          requireToolCall = false
        )
      )
    )
