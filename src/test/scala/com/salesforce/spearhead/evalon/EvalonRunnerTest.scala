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
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.*

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

import com.salesforce.spearhead.evalon.agent.{Agent, AgentReply, SimpleAgent}
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

  // A judge LLM that scores every run "ok", and drives the simulated user to say something
  // (so the evaluated agent actually gets a turn) rather than ending immediately.
  private val judgeAndChatLlm: Llm = prompt =>
    val text =
      if prompt.contains("expert evaluator") then
        """{"passed":true,"reasoning":"ok"}"""
      else "I need help with my order."
    CompletableFuture.completedFuture(text)

  private val failFastOptions = EvalonRunOptions()
    .withSimulationTimeout(Duration.ofSeconds(15))
    .withJudgeTimeout(Duration.ofSeconds(5))

  test("agent step returning null fails the run instead of being swallowed") {
    val agent: SimpleAgent = (_, _, _) => null
    val ex = intercept[AgentStepFailedException] {
      evalon.runSimple(scenario("null-step"), agent, judgeAndChatLlm, failFastOptions)
    }
    assert(ex.getCause.isInstanceOf[NullPointerException])
  }

  test("agent step throwing fails the run") {
    val agent: SimpleAgent = (_, _, _) => throw new IllegalStateException("boom")
    val ex = intercept[AgentStepFailedException] {
      evalon.runSimple(scenario("throwing-step"), agent, judgeAndChatLlm, failFastOptions)
    }
    assert(ex.getCause.getMessage == "boom")
  }

  test("agent step completing exceptionally fails the run") {
    val agent: SimpleAgent = (_, _, _) =>
      CompletableFuture.failedFuture[AgentReply](new IllegalStateException("boom"))
    val ex = intercept[AgentStepFailedException] {
      evalon.runSimple(scenario("failed-stage"), agent, judgeAndChatLlm, failFastOptions)
    }
    assert(ex.getCause.getMessage == "boom")
  }

  test("a successful non-empty send is recorded and the run is scored") {
    val calls = new AtomicInteger()
    val agent: SimpleAgent = (_, _, _) =>
      val reply =
        if calls.getAndIncrement() == 0 then AgentReply.send("Here is your answer.")
        else AgentReply.end()
      CompletableFuture.completedFuture(reply)

    val result = evalon.runSimple(scenario("send"), agent, judgeAndChatLlm, failFastOptions)

    assert(result.getSummary == "ok")
    assert(result.scalaTranscript.messages.exists { m =>
      m.sender == "agent" && m.content == "Here is your answer."
    })
  }

  test("a successful empty send is treated as silence, not an agent failure") {
    // Empty content is a real "say nothing": it must not become AgentStepFailedException.
    // With nothing delivered, the conversation stalls and the run times out instead.
    val agent: SimpleAgent = (_, _, _) => CompletableFuture.completedFuture(AgentReply.send(""))
    val options = EvalonRunOptions()
      .withSimulationTimeout(Duration.ofSeconds(2))
      .withJudgeTimeout(Duration.ofSeconds(2))

    val thrown = intercept[Throwable] {
      evalon.runSimple(scenario("empty-send"), agent, judgeAndChatLlm, options)
    }
    assert(!thrown.isInstanceOf[AgentStepFailedException])
  }

  test("a simulated participant's LLM failure fails the run as a harness failure") {
    // The failing participant is not the agent under test, so it surfaces as a harness failure.
    val llm: Llm = _ => CompletableFuture.failedFuture(new IllegalStateException("llm down"))
    val agent: SimpleAgent = (_, _, _) => CompletableFuture.completedFuture(AgentReply.end())

    val ex = intercept[SimulationFailedException] {
      evalon.runSimple(scenario("sim-llm-fail"), agent, llm, failFastOptions)
    }
    assert(ex.getParticipant == "end_user")
    assert(ex.getCause.getMessage == "llm down")
  }

  test("an event source's LLM failure fails the run as a harness failure") {
    val eventSource = EventSourceConfig(
      name = "case_events",
      sourceType = "simulated",
      description = "a case management system",
      emits = List(EventEmitConfig("case_created", Map("id" -> "string")))
    )
    // Fail only the event source's LLM call; keep the user and agent talking (never ending) so the
    // event-source failure is the only thing that can stop the run.
    val llm: Llm = prompt =>
      if prompt.contains("simulated event source") then
        CompletableFuture.failedFuture(new IllegalStateException("event source down"))
      else CompletableFuture.completedFuture("I need help with my order.")
    val agent: SimpleAgent =
      (_, _, _) => CompletableFuture.completedFuture(AgentReply.send("Working on it."))

    val ex = intercept[SimulationFailedException] {
      evalon.runSimple(scenario("es-llm-fail", List(eventSource)), agent, llm, failFastOptions)
    }
    assert(ex.getParticipant == "case_events")
    assert(ex.getCause.getMessage == "event source down")
  }

  // A raw Agent (implementing the trait directly, not via the SimpleAgent bridge) whose step body
  // is by-name, so it can throw synchronously or return null before yielding a Future.
  private def rawAgent(body: => Future[Action]): Agent =
    new Agent:
      def step(h: List[HistoryEntry], e: List[Event], r: String): Future[Action] = body

  test("a raw agent whose step throws synchronously fails the run") {
    // The throw happens before a Future exists, so pipeToSelf never sees it. It must still be
    // caught and surfaced, not left to kill the actor and stall the run.
    val agent = rawAgent(throw new IllegalStateException("sync boom"))
    val ex = intercept[AgentStepFailedException] {
      evalon.run(scenario("raw-sync-throw"), agent, judgeAndChatLlm, failFastOptions)
    }
    assert(ex.getCause.getMessage == "sync boom")
  }

  test("a raw agent that returns a null future fails the run") {
    val agent = rawAgent(null)
    val ex = intercept[AgentStepFailedException] {
      evalon.run(scenario("raw-null"), agent, judgeAndChatLlm, failFastOptions)
    }
    assert(ex.getCause.isInstanceOf[NullPointerException])
  }

  test("a raw agent whose future completes with a null action fails the run") {
    // Distinct from a null future: the future is fine but yields a null action, which would
    // otherwise MatchError in recordAction. It must be surfaced as a step failure.
    val agent = rawAgent(Future.successful(null))
    val ex = intercept[AgentStepFailedException] {
      evalon.run(scenario("raw-null-action"), agent, judgeAndChatLlm, failFastOptions)
    }
    assert(ex.getCause.isInstanceOf[NullPointerException])
  }

  test("a participant LLM that throws synchronously fails the run as a harness failure") {
    val llm: Llm = _ => throw new IllegalStateException("sync llm boom")
    val agent: SimpleAgent = (_, _, _) => CompletableFuture.completedFuture(AgentReply.end())
    val ex = intercept[SimulationFailedException] {
      evalon.runSimple(scenario("raw-sync-llm"), agent, llm, failFastOptions)
    }
    assert(ex.getParticipant == "end_user")
    assert(ex.getCause.getMessage == "sync llm boom")
  }

  test("a participant LLM that returns null fails the run as a harness failure") {
    val llm: Llm = _ => null
    val agent: SimpleAgent = (_, _, _) => CompletableFuture.completedFuture(AgentReply.end())
    val ex = intercept[SimulationFailedException] {
      evalon.runSimple(scenario("raw-null-llm"), agent, llm, failFastOptions)
    }
    assert(ex.getParticipant == "end_user")
    assert(ex.getCause.isInstanceOf[NullPointerException])
  }

  private def scenario(
      name: String,
      eventSources: List[EventSourceConfig] = Nil
  ): Scenario =
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
      eventSources = eventSources,
      evalCriteria = List(
        EvalCriterion(
          name = "ok",
          description = "ok",
          criterionType = CriterionType.Binary,
          requireToolCall = false
        )
      )
    )
