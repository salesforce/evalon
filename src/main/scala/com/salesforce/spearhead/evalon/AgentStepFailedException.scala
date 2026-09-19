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

/** Thrown by {@link EvalonRunner#run} when the evaluated agent's {@code step} fails (a thrown
  * exception, a null or failed {@code CompletionStage}, or exhausted retries). The failure is
  * surfaced instead of being swallowed as an empty message, so a broken agent fails the run rather
  * than being silently judged.
  *
  * @param cause the underlying step failure
  */
final class AgentStepFailedException(cause: Throwable, transcript: Transcript)
    extends RuntimeException("Evaluated agent step failed during simulation", cause):

  /** The partial transcript recorded before the failure, for debugging. */
  def getTranscript: Transcript = transcript
