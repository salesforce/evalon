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

import com.salesforce.spearhead.evalon.model.Transcript

/** Thrown by {@link EvalonRunner#run} when a simulation-driving participant (a simulated
  * participant or an event source) fails terminally, e.g. its LLM call errors after retries. This
  * is a broken harness, distinct from {@link AgentStepFailedException}, which signals that the
  * agent under test failed. Callers can catch this separately to retry a flaky run rather than
  * attributing the failure to the agent.
  *
  * @param participant the name of the participant whose work failed
  * @param cause the underlying failure
  */
final class SimulationFailedException(participant: String, cause: Throwable, transcript: Transcript)
    extends RuntimeException(s"Simulation participant '$participant' failed", cause):

  /** The name of the participant whose work failed. */
  def getParticipant: String = participant

  /** The partial transcript recorded before the failure, for debugging. */
  def getTranscript: Transcript = transcript
