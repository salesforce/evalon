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
import com.salesforce.spearhead.evalon.llm.Llm
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
  private val userLlm: Llm = (_, _) => CompletableFuture.completedFuture("I need help.")

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

  test("a stale idle report does not end the run") {
    // The user's LLM call never finishes, so the user is busy with the kickoff it was delivered.
    val pending = CompletableFuture[String]()
    val llm: Llm = (_, _) => pending
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
    val llm: Llm = (system, messages) =>
      if system.contains("simulated event source") then sourceCall
      else userLlm.completeChat(system, messages)
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
    val llm: Llm = (system, messages) =>
      if system.contains("simulated event source") then
        CompletableFuture.completedFuture("""[{"name":"all_waiting","data":{}}]""")
      else userLlm.completeChat(system, messages)
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

  test("a run with two evaluated participants still completes as idle") {
    // Both receive the user's message. Each must report idle under its own name, or the runner's
    // count for the second never matches and the run hangs.
    val base = scenario()
    val twoAgents = base.copy(
      participants = Map(
        "end_user" -> ParticipantConfig("end_user", ParticipantType.Simulated),
        "agent" -> ParticipantConfig("agent", ParticipantType.Evaluated),
        "supervisor" -> ParticipantConfig("supervisor", ParticipantType.Evaluated)
      ),
      conversations = List(
        ConversationConfig(
          "chat",
          List("end_user", "agent", "supervisor"),
          initiatedBy = Some("end_user")
        )
      )
    )
    val agent = ScriptedAgent((_, _) => silent)
    val (_, probe) = run(twoAgents, agent, userLlm)
    assert(expectCompleted(probe)._2 == StopReason.Idle)
  }

  test("[END] and max_turns report their stop reasons") {
    val ending = ScriptedAgent((_, _) => Action.End)
    val (_, endProbe) = run(scenario(), ending, userLlm)
    assert(expectCompleted(endProbe)._2 == StopReason.End)

    val talking = ScriptedAgent((_, _) => Action.send("agent", "How can I help?"))
    val (_, turnsProbe) = run(scenario(maxTurns = 1), talking, userLlm)
    assert(expectCompleted(turnsProbe)._2 == StopReason.MaxTurns)
  }
