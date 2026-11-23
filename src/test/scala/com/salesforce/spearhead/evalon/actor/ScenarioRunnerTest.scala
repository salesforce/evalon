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

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{CompletableFuture, LinkedBlockingQueue}

import scala.concurrent.Future
import scala.concurrent.duration.*

import org.apache.pekko.actor.testkit.typed.scaladsl.{ActorTestKit, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

import com.salesforce.spearhead.evalon.actor.ScenarioRunner.SimulationResult
import com.salesforce.spearhead.evalon.agent.Agent
import com.salesforce.spearhead.evalon.llm.{Llm, TestLlm}
import com.salesforce.spearhead.evalon.model.*

class ScenarioRunnerTest extends AnyFunSuite with BeforeAndAfterAll:

  private val testKit = ActorTestKit()

  override def afterAll(): Unit = testKit.shutdownTestKit()

  private def scenario(
      maxTurns: Int = 10,
      eventSources: List[EventSourceConfig] = Nil
  ): Scenario =
    Scenario(
      name = "idle-test",
      description = "idle detection",
      participants = Map(
        "end_user" -> ParticipantConfig("end_user", ParticipantType.Simulated),
        "agent" -> ParticipantConfig("agent", ParticipantType.Evaluated)
      ),
      conversations = List(
        ConversationConfig("chat", List("end_user", "agent"), initiatedBy = Some("end_user"))
      ),
      eventSources = eventSources,
      maxTurns = maxTurns
    )

  /** Records the events of every step and answers with `reply(stepIndex, events)`. */
  private class ScriptedAgent(reply: (Int, List[Event]) => Action) extends Agent:
    val steps = LinkedBlockingQueue[List[Event]]()
    private val count = AtomicInteger()
    def step(history: List[HistoryEntry], events: List[Event], respondIn: String): Future[Action] =
      steps.put(events)
      Future.successful(reply(count.getAndIncrement(), events))
    def allWaitingSeen: Int =
      steps.toArray.toList.map(_.asInstanceOf[List[Event]]).count(_.exists(_.name == "all_waiting"))

  private val silent = Action.send("agent", "")

  /** The simulated user always says the same thing. */
  private val userLlm: Llm = TestLlm((_, _) => CompletableFuture.completedFuture("I need help."))

  private def run(scenario: Scenario, agent: Agent, llm: Llm) =
    val probe = testKit.createTestProbe[SimulationResult]()
    val runner = testKit.spawn(ScenarioRunner(scenario, agent, llm))
    runner ! ScenarioRunner.Run(probe.ref)
    (runner, probe)

  private def expectCompleted(probe: TestProbe[SimulationResult]): (Transcript, StopReason) =
    probe.expectMessageType[SimulationResult.Completed](5.seconds) match
      case SimulationResult.Completed(t, reason) => (t, reason)

  test("a stalled run gets one all_waiting event, then completes as idle") {
    // The agent never says anything, so the user is never prompted again: the #29 stall.
    val agent = ScriptedAgent((_, _) => silent)
    val (_, probe) = run(scenario(), agent, userLlm)

    val (transcript, reason) = expectCompleted(probe)
    assert(reason == StopReason.Idle)
    assert(agent.allWaitingSeen == 1)
    assert(
      !transcript.entries.exists(_.event.exists(_.name == "all_waiting")),
      "all_waiting is not recorded"
    )
  }

  test("all_waiting is re-armed once something new is posted") {
    // The agent answers the first all_waiting, which wakes the user; the next quiet period gets a
    // second one, and the agent's silence after that ends the run.
    val idleAnswers = AtomicInteger()
    val agent = ScriptedAgent { (_, events) =>
      val allWaiting = events.exists(_.name == "all_waiting")
      if allWaiting && idleAnswers.getAndIncrement() == 0 then
        Action.send("agent", "Anything else?")
      else silent
    }
    val (_, probe) = run(scenario(), agent, userLlm)

    val (_, reason) = expectCompleted(probe)
    assert(reason == StopReason.Idle)
    assert(agent.allWaitingSeen == 2)
  }

  test("an actor that has never reported counts as busy, even with nothing delivered") {
    // A timer-driven source never reports. Defaulting its missing report to zero would match its
    // zero deliveries and let the run end before the timer fires.
    val names = Set("agent", "timer_source")
    assert(!ScenarioRunner.allIdle(names, delivered = Map.empty, reported = Map("agent" -> 0)))
    assert(ScenarioRunner.allIdle(names, Map.empty, Map("agent" -> 0, "timer_source" -> 0)))
    assert(
      !ScenarioRunner.allIdle(names, Map("agent" -> 1), Map("agent" -> 0, "timer_source" -> 0))
    )
  }

  test("a run with no initiator gets all_waiting from the start reports") {
    val noInitiator = scenario().copy(
      conversations = List(ConversationConfig("chat", List("end_user", "agent")))
    )
    val agent = ScriptedAgent((_, _) => silent)
    val (_, probe) = run(noInitiator, agent, userLlm)
    assert(expectCompleted(probe)._2 == StopReason.Idle)
    assert(agent.allWaitingSeen == 1)
  }

  test("a runner for a scenario with no evaluated participant is rejected when built") {
    val empty = scenario().copy(participants = Map.empty, conversations = Nil)
    val ex = intercept[IllegalArgumentException] {
      ScenarioRunner(empty, ScriptedAgent((_, _) => silent), userLlm)
    }
    assert(ex.getMessage.contains("no evaluated participant"))
  }

  test("a stale idle report does not end the run") {
    // The user's LLM call never finishes, so the user is busy with the kickoff it was delivered.
    val pending = CompletableFuture[String]()
    val llm: Llm = TestLlm((_, _) => pending)
    val agent = ScriptedAgent((_, _) => silent)
    val (runner, probe) = run(scenario(), agent, llm)

    // A report predating the kickoff delivery: processed 0 of 1.
    runner ! ScenarioRunner.Idle("end_user", 0)
    probe.expectNoMessage(300.millis)
    assert(agent.steps.isEmpty, "no all_waiting while the user is still working")
    pending.complete(Signals.End)
    assert(expectCompleted(probe)._2 == StopReason.End)
  }

  test("an event source with an LLM call in flight keeps the run going") {
    val sourceCall = CompletableFuture[String]()
    val llm: Llm = TestLlm: (system, messages) =>
      if system.contains("simulated event source") then sourceCall
      else CompletableFuture.completedFuture("I need help.")
    val source = EventSourceConfig("case_events", "simulated", "a case system")
    val agent = ScriptedAgent((_, _) => silent)
    val (_, probe) = run(scenario(eventSources = List(source)), agent, llm)

    probe.expectNoMessage(500.millis)
    assert(agent.allWaitingSeen == 0)
    sourceCall.complete("[]")
    assert(expectCompleted(probe)._2 == StopReason.Idle)
  }

  test("an event source cannot impersonate all_waiting") {
    // The source emits a fake all_waiting with the user's first message. If it reached the agent,
    // the agent would answer it, and the run would look like it had gone idle when it hadn't.
    val llm: Llm = TestLlm: (system, messages) =>
      if system.contains("simulated event source") then
        CompletableFuture.completedFuture("""[{"name":"all_waiting","data":{}}]""")
      else CompletableFuture.completedFuture("I need help.")
    val source = EventSourceConfig("case_events", "simulated", "a case system")
    val observing = scenario(eventSources = List(source)).copy(
      observations = List(ObservationConfig("agent", List("case_events")))
    )
    val agent = ScriptedAgent((_, _) => silent)
    val (_, probe) = run(observing, agent, llm)

    val (transcript, reason) = expectCompleted(probe)
    assert(reason == StopReason.Idle)
    assert(agent.allWaitingSeen == 1, "only the runner's own all_waiting reaches the agent")
    assert(!transcript.entries.exists(_.event.exists(_.name == Event.AllWaiting)))
  }

  test("a simulated participant whose config name differs from its key still completes as idle") {
    // Scenarios built in code can map "end_user" to a config named "customer". The runner counts
    // deliveries under the key, so idle reports must use the key too.
    val renamed = scenario().copy(
      participants = Map(
        "end_user" -> ParticipantConfig("customer", ParticipantType.Simulated),
        "agent" -> ParticipantConfig("agent", ParticipantType.Evaluated)
      )
    )
    val agent = ScriptedAgent((_, _) => silent)
    val (_, probe) = run(renamed, agent, userLlm)
    val (transcript, reason) = expectCompleted(probe)
    assert(reason == StopReason.Idle)
    assert(transcript.messages.exists(_.sender == "end_user"))
    assert(!transcript.messages.exists(_.sender == "customer"))
  }

  test("a runner for two evaluated participants is rejected when built, not run as one") {
    // Direct callers bypass EvalonRunner's validation. Throwing when the behavior is built, not
    // when it starts, also keeps a caller from waiting on a runner that never replies.
    val base = scenario()
    val twoAgents = base.copy(participants =
      base.participants.updated(
        "supervisor",
        ParticipantConfig("supervisor", ParticipantType.Evaluated)
      )
    )
    val ex = intercept[IllegalArgumentException] {
      ScenarioRunner(twoAgents, ScriptedAgent((_, _) => silent), userLlm)
    }
    assert(ex.getMessage.contains("only one evaluated participant"))
  }

  test("[END] and max_turns report their stop reasons") {
    val ending = ScriptedAgent((_, _) => Action.End)
    val (_, endProbe) = run(scenario(), ending, userLlm)
    assert(expectCompleted(endProbe)._2 == StopReason.End)

    val talking = ScriptedAgent((_, _) => Action.send("agent", "How can I help?"))
    val (_, turnsProbe) = run(scenario(maxTurns = 1), talking, userLlm)
    assert(expectCompleted(turnsProbe)._2 == StopReason.MaxTurns)
  }
