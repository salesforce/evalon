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
          """{"criteria":[{"passed":true,"score":1.0,"reasoning":"ok"}],"summary":"ok"}"""
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
      evalCriteria = List(EvalCriterion("ok", 1.0))
    )
