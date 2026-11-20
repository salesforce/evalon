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

package com.salesforce.spearhead.evalon.model

/**
 * Why a simulation that completed normally stopped. A Java enum, so Java callers can switch on it.
 */
enum StopReason extends java.lang.Enum[StopReason]:
  /** A participant signalled [END]. */
  case End

  /** A conversation reached the scenario's `max_turns`. */
  case MaxTurns

  /**
   * Every participant and event source was idle, the evaluated agent was told so, and nothing new
   * was posted afterward.
   */
  case Idle
