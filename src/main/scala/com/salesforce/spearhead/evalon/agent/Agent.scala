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

package com.salesforce.spearhead.evalon.agent

import scala.concurrent.Future

import com.salesforce.spearhead.evalon.model.{Action, Event, HistoryEntry}

/** Protocol that any agent must implement to be evaluated.
  *
  * Stateless across calls — the simulator owns all conversation history (including the agent's
  * own outgoing messages and tool work) and event accumulation, passing the full slice on every
  * step. The simulator also chooses which conversation the response is addressed to. The agent
  * runs its own tools and returns the resulting action plus the tool trace for the current step.
  */
trait Agent:

  /** Produce the agent's next action for `respondIn`, given the history and pending events.
    *
    * Must not block the calling thread. `step` is invoked inline on the simulator's actor
    * dispatcher, so any blocking work (network, disk, `Await`) done synchronously before the
    * returned `Future` completes stalls the actor and ties up a dispatcher thread. Do I/O
    * asynchronously, or run blocking work on your own `ExecutionContext` and return a `Future`
    * that completes when it finishes. A synchronous throw or a null result is treated as a step
    * failure and fails the run.
    */
  def step(
      history: List[HistoryEntry],
      events: List[Event],
      respondIn: String
  ): Future[Action]
