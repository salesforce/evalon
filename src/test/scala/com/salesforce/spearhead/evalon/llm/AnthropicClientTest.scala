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

package com.salesforce.spearhead.evalon.llm

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.jdk.FutureConverters.*

import io.circe.Json
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite

class AnthropicClientTest extends AnyFunSuite:

  private given ExecutionContext = ExecutionContext.global

  /** A client whose API always returns `content` with `stopReason`. */
  private def stubClient(stopReason: String, content: ContentBlock*): AnthropicClient =
    new AnthropicClient("http://unused", "unused", backend = null):
      override def createMessage(request: CreateMessageRequest): Future[CreateMessageResponse] =
        Future.successful(CreateMessageResponse("resp", content.toList, stopReason, Usage(0, 0)))

  private def complete(client: AnthropicClient) =
    Await.ready(client.complete(LlmRequest("sys", Nil)).asScala, 5.seconds).value.get

  private val sendArgs = Json.obj("conversation" -> "chat".asJson, "text" -> "Hi".asJson)
  private val sendCall = ContentBlock.ToolUseBlock("call-1", "send_message", sendArgs)

  test("a reply cut off by max_tokens fails even after a complete tool call") {
    // The calls that would have followed are lost, so acting on the first would be acting on part
    // of a turn.
    assert(complete(stubClient("max_tokens", sendCall)).isFailure)
  }

  test("a reply that stopped for any reason but finishing fails, naming the reason") {
    // A context-window cutoff reads as silence just like max_tokens; a refusal isn't an answer.
    List("max_tokens", "model_context_window_exceeded", "refusal", "pause_turn", "stop_sequence")
      .foreach { reason =>
        val result = complete(stubClient(reason, ContentBlock.TextBlock("partial")))
        assert(result.failed.get.getMessage.contains(reason), reason)
      }
  }

  test("a reply that ended its turn returns its text") {
    assert(complete(stubClient("end_turn", ContentBlock.TextBlock("Done."))).get.text == "Done.")
  }

  test("a finished reply returns its text and tool calls") {
    val result = complete(stubClient("tool_use", ContentBlock.TextBlock("Sure."), sendCall))
    assert(result.get == LlmResponse("Sure.", List(LlmToolCall("send_message", sendArgs))))
  }
