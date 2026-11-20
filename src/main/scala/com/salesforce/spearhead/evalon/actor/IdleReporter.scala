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

import org.apache.pekko.actor.typed.ActorRef

/**
 * Counts the runner deliveries an actor has handled and reports the count when the actor becomes
 * idle, which is how the runner detects that a run has stalled (see [[ScenarioRunner.Idle]]).
 *
 * Call `received()` for every command the runner sends this actor, in every state. Call
 * `reportIdle()` whenever the actor is left with nothing to do and will not act again unless the
 * runner sends it something new: when it starts, after work finishes, and after handling a delivery
 * that starts no work. LLM results, step results and timers are the actor's own and are not
 * counted.
 *
 * The runner treats an actor that has never reported as busy. So the report at start is required
 * for an actor that only acts on deliveries, and an actor that can act on its own (a timer, an
 * external feed) must never report, not even at start.
 *
 * Holds mutable state, so create one per actor instance and use it only from that actor.
 */
final private[actor] class IdleReporter(name: String, runner: ActorRef[ScenarioRunner.Command]):
  private var processed = 0
  def received(): Unit = processed += 1
  def reportIdle(): Unit = runner ! ScenarioRunner.Idle(name, processed)
