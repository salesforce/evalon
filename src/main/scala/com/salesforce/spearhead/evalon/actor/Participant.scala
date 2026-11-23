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

import com.salesforce.spearhead.evalon.model.{Event, Message}

/** Shared protocol for all participant actors (simulated, evaluated, custom). */
object Participant:

  sealed trait Command
  case class ReceiveMessage(message: Message, conversation: String) extends Command
  case class ReceiveEvents(events: List[Event]) extends Command

  /**
   * The runner opening `conversation` for its initiator. A command of its own, not a message, so no
   * participant's message (even from one named "system") can be taken for a kickoff.
   */
  case class Kickoff(conversation: String) extends Command

  /** How a kickoff reads in a history of messages, for participants that keep one. */
  val KickoffMessage: Message = Message("system", "The conversation is starting. Please begin.")
