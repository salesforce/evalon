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

import java.nio.file.Path
import java.util.concurrent.atomic.AtomicLong

import scala.concurrent.duration.*
import scala.concurrent.Await
import scala.jdk.DurationConverters.*

import org.apache.pekko.actor.typed.{ActorRef, ActorSystem, Props, SpawnProtocol}
import org.apache.pekko.actor.typed.scaladsl.AskPattern.*
import org.apache.pekko.util.Timeout

import com.salesforce.spearhead.evalon.actor.ScenarioRunner
import com.salesforce.spearhead.evalon.agent.{Agent, SimpleAgent}
import com.salesforce.spearhead.evalon.eval.Evaluator
import com.salesforce.spearhead.evalon.llm.Llm
import com.salesforce.spearhead.evalon.model.{ParticipantType, Scenario}
import com.salesforce.spearhead.evalon.scenario.ScenarioLoader

/** Blocking Java-callable entry point: load a scenario, run Pekko simulation, judge.
  *
  * Uses one shared {@code ActorSystem}. Each {@link #run} spawns a {@code ScenarioRunner} child.
  * Call {@link #shutdown} when the host process is finished so the JVM can exit.
  */
object EvalonRunner:

  private val lock = new AnyRef
  private val runSeq = new AtomicLong()
  private var instance: Option[ActorSystem[SpawnProtocol.Command]] = None

  def run(scenarioPath: Path, agent: Agent, llm: Llm): EvalonResult =
    run(scenarioPath, agent, llm, EvalonRunOptions())

  def run(scenarioPath: Path, agent: Agent, llm: Llm, options: EvalonRunOptions): EvalonResult =
    run(loadScenario(scenarioPath), agent, llm, options)

  def run(scenarioPath: Path, agent: SimpleAgent, llm: Llm): EvalonResult =
    run(scenarioPath, agent, llm, EvalonRunOptions())

  def run(
      scenarioPath: Path,
      agent: SimpleAgent,
      llm: Llm,
      options: EvalonRunOptions
  ): EvalonResult =
    val scenario = loadScenario(scenarioPath)
    val agentName = evaluatedName(scenario)
    val agentImpl = SimpleAgent.toAgent(agent, agentName)(using options.executionContext)
    run(scenario, agentImpl, llm, options)

  def run(scenario: Scenario, agent: SimpleAgent, llm: Llm, options: EvalonRunOptions): EvalonResult =
    val agentName = evaluatedName(scenario)
    val agentImpl = SimpleAgent.toAgent(agent, agentName)(using options.executionContext)
    run(scenario, agentImpl, llm, options)

  def run(scenario: Scenario, agent: Agent, llm: Llm, options: EvalonRunOptions): EvalonResult =
    val ec = options.executionContext
    val timeout = Timeout(options.getSimulationTimeout.toScala)
    val system = actorSystem
    val runName = s"run-${runSeq.incrementAndGet()}"

    val runner = Await.result(
      system.ask[ActorRef[ScenarioRunner.Command]] { replyTo =>
        SpawnProtocol.Spawn(
          ScenarioRunner(scenario, agent, llm, options.onEntryFn),
          runName,
          Props.empty,
          replyTo
        )
      }(using timeout, system.scheduler),
      options.getSimulationTimeout.toScala
    )

    val simulation = Await.result(
      runner.ask[ScenarioRunner.SimulationResult](ref => ScenarioRunner.Run(ref))(
        using timeout,
        system.scheduler
      ),
      options.getSimulationTimeout.toScala
    )
    val evalResult = Await.result(
      Evaluator(llm)(using ec).evaluate(scenario, simulation.transcript),
      options.getJudgeTimeout.toScala
    )
    EvalonResult.from(evalResult, simulation.transcript)

  /** Stop the shared actor system. Safe to call more than once. */
  def shutdown(): Unit =
    lock.synchronized {
      instance.foreach { sys =>
        sys.terminate()
        Await.ready(sys.whenTerminated, 30.seconds)
      }
      instance = None
    }

  private def actorSystem: ActorSystem[SpawnProtocol.Command] =
    lock.synchronized {
      instance.filter(sys => !sys.whenTerminated.isCompleted) match
        case Some(sys) => sys
        case None =>
          val created = ActorSystem(SpawnProtocol(), "evalon")
          instance = Some(created)
          created
    }

  private def loadScenario(path: Path): Scenario =
    ScenarioLoader.load(path) match
      case Right(s) => s
      case Left(e)  => throw RuntimeException(s"Failed to load scenario: $e")

  private def evaluatedName(scenario: Scenario): String =
    scenario.participants
      .find(_._2.participantType == ParticipantType.Evaluated)
      .map(_._1)
      .getOrElse("agent")
