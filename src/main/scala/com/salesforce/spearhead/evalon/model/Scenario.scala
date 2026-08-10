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

import io.circe.*

enum ParticipantType:
  case Simulated, Evaluated, Custom

enum ResponseSpeed:
  case Fast, Medium, Slow

case class ParticipantConfig(
  name: String,
  participantType: ParticipantType = ParticipantType.Simulated,
  persona: String = "",
  goal: String = "",
  endpoint: Option[String] = None,
  responseSpeed: Option[ResponseSpeed] = None,
)

case class ConversationConfig(
  name: String,
  between: List[String],
  initiatedBy: Option[String] = None,
)

case class ObservationConfig(
  participant: String,
  observes: List[String],
)

case class EventEmitConfig(
  event: String,
  schema: Map[String, String] = Map.empty,
)

case class EventSourceConfig(
  name: String,
  sourceType: String, // "simulated" or "custom"
  description: String = "",
  emits: List[EventEmitConfig] = Nil,
  className: Option[String] = None,
)

case class EvalCriterion(
  description: String,
  weight: Double = 1.0,
)

case class Scenario(
  name: String,
  description: String,
  participants: Map[String, ParticipantConfig],
  conversations: List[ConversationConfig],
  observations: List[ObservationConfig] = Nil,
  eventSources: List[EventSourceConfig] = Nil,
  context: Json = Json.obj(),
  evalCriteria: List[EvalCriterion] = Nil,
  maxTurns: Int = 20,
)
