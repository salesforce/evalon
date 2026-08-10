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

import io.circe.*
import io.circe.syntax.*

import com.salesforce.spearhead.evalon.model.{Action, Event, HistoryEntry}

/** Wire format for `POST /v1/step`.
  *
  * Mirrors the in-process `Agent.step(history, events, respondIn)` signature with a
  * `protocol_version` field for future-proofing.
  */
case class StepRequest(
  protocolVersion: String,
  history: List[HistoryEntry],
  events: List[Event],
  respondIn: String,
)

object StepRequest:
  val currentVersion: String = "1"

  given Encoder[StepRequest] = Encoder.instance { r =>
    Json.obj(
      "protocol_version" -> r.protocolVersion.asJson,
      "history" -> r.history.asJson,
      "events" -> r.events.asJson,
      "respond_in" -> r.respondIn.asJson,
    )
  }

case class StepResponse(action: Action)

object StepResponse:
  given Decoder[StepResponse] = Decoder.instance { c =>
    c.downField("action").as[Action].map(StepResponse(_))
  }
