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

import scala.util.{Failure, Success}

import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.typed.scaladsl.Behaviors

import com.salesforce.spearhead.evalon.llm.{Llm, ChatMessage}
import com.salesforce.spearhead.evalon.model.{Event, EventSourceConfig, TranscriptEntry}

/** Event source actor — observes simulation traffic and emits derived events.
  *
  * Simulated event sources use an LLM to reason about whether new events should be fired.
  */
object EventSourceActor:

  sealed trait Command
  case class SimulationUpdate(entry: TranscriptEntry) extends Command

  // Internal
  private case class EmissionResult(events: List[Event]) extends Command

  def apply(
    config: EventSourceConfig,
    llm: Llm,
    runner: ActorRef[ScenarioRunner.Command],
  ): Behavior[Command] = simulated(config, llm, runner, Vector.empty)

  private val systemPromptTemplate: String =
    """You are a simulated event source in a scenario simulation.

You represent: {description}

You can emit these event types:
{emits}

You observe all messages and events in the simulation. Based on what you observe, decide if any events should be fired. Respond with a JSON array of events to emit, or an empty array if none.

Format: [{"name": "event_type", "data": {"key": "value"}}]

Only emit events when clearly warranted by observed activity. Do not emit events proactively without cause."""

  private def simulated(
    config: EventSourceConfig,
    llm: Llm,
    runner: ActorRef[ScenarioRunner.Command],
    observedEntries: Vector[TranscriptEntry],
  ): Behavior[Command] = Behaviors.receive { (context, message) =>
    message match
      case SimulationUpdate(entry) =>
        val updated = observedEntries :+ entry

        // Only reason about events when we see a new message (not tool results, etc.)
        if entry.message.isDefined then
          val emitsDesc = config.emits.map { e =>
            s"- ${e.event}: ${e.schema.map((k, v) => s"$k: $v").mkString(", ")}"
          }.mkString("\n")

          val systemPrompt = systemPromptTemplate
            .replace("{description}", config.description)
            .replace("{emits}", emitsDesc)

          val activityLog = updated.flatMap { e =>
            e.message.map(m => s"[${m.sender}]: ${m.content}")
              .orElse(e.event.map(ev => s"[event: ${ev.name}] ${ev.data.asJson.noSpaces}"))
          }.mkString("\n")

          val userPrompt = s"Activity so far:\n$activityLog\n\nShould any events be emitted?"
          context.pipeToSelf(llm.completeAsync(systemPrompt, List(ChatMessage.user(userPrompt)))) {
            case Success(response) =>
              val text = Option(response).filter(_.nonEmpty).getOrElse("[]")
              val events = parse(text).flatMap(_.as[List[Json]]).getOrElse(Nil).flatMap { j =>
                for
                  name <- j.hcursor.downField("name").as[String].toOption
                  data <- j.hcursor.downField("data").as[Map[String, Json]].toOption
                yield Event(name, data)
              }
              EmissionResult(events)
            case Failure(e) =>
              context.log.warn("Event source LLM call failed: {}", e.getMessage)
              EmissionResult(Nil)
          }
          simulated(config, llm, runner, updated)
        else simulated(config, llm, runner, updated)

      case EmissionResult(events) =>
        if events.nonEmpty then
          runner ! ScenarioRunner.EventSourceEmission(config.name, events)
        Behaviors.same
  }
