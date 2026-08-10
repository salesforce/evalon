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

import scala.concurrent.{ExecutionContext, Future}

import io.circe.parser.*
import io.circe.syntax.*
import sttp.client4.*

import com.salesforce.spearhead.evalon.model.{Action, Event, HistoryEntry}

/** Proxies the Agent trait over HTTP to a remote implementation in any language.
  *
  * Stateless: every `step` call serializes the full history + events + target conversation and
  * POSTs to the configured endpoint. No internal buffers. The remote server runs its own tools
  * and returns the resulting action plus the tool trace.
  */
class RemoteAgent(
    endpoint: String,
    backend: Backend[Future],
)(using ec: ExecutionContext)
    extends Agent:

  private val retryableStatusCodes = Set(429, 502, 503, 529)
  private val maxRetries = 3
  private val baseDelayMs = 1000

  override def step(
      history: List[HistoryEntry],
      events: List[Event],
      respondIn: String,
  ): Future[Action] =
    val request = StepRequest(
      protocolVersion = StepRequest.currentVersion,
      history = history,
      events = events,
      respondIn = respondIn,
    )
    sendWithRetry(request.asJson.noSpaces)

  private def sendWithRetry(
      requestBody: String,
      attempt: Int = 0,
  ): Future[Action] =
    basicRequest
      .post(uri"$endpoint/v1/step")
      .header("content-type", "application/json")
      .body(requestBody)
      .response(asString)
      .send(backend)
      .flatMap { response =>
        response.body match
          case Right(body) =>
            parse(body).flatMap(_.as[StepResponse]) match
              case Right(StepResponse(action)) => Future.successful(action)
              case Left(err) =>
                Future.failed(
                  RuntimeException(s"Failed to parse remote agent response: $err\nBody: $body")
                )
          case Left(errorBody)
              if retryableStatusCodes.contains(response.code.code) && attempt < maxRetries =>
            val delay = baseDelayMs * (1 << attempt)
            Future {
              Thread.sleep(delay)
            }.flatMap(_ => sendWithRetry(requestBody, attempt + 1))
          case Left(errorBody) =>
            Future.failed(
              RuntimeException(s"Remote agent error (${response.code}): $errorBody")
            )
      }
