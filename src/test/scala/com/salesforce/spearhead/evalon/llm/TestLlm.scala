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

import java.util.concurrent.CompletionStage

import scala.concurrent.ExecutionContext
import scala.jdk.FutureConverters.*

import io.circe.Json
import io.circe.syntax.*

import com.salesforce.spearhead.evalon.model.Signals

/**
 * Builds an [[Llm]] from a text reply, for tests that don't care about tool calls. Calls without
 * tools (the judge, event sources) get the text as is. For a simulated participant's turn the text
 * becomes its action: empty is silence, `[END]` calls `end_conversation`, anything else is sent to
 * the first conversation it may post in.
 */
object TestLlm:
  def apply(reply: (String, List[ChatMessage]) => CompletionStage[String]): Llm = request =>
    // Via a Scala future: thenApply would wrap a failure in CompletionException.
    reply(request.system, request.messages).asScala
      .map(text => asTurn(request, text))(using ExecutionContext.parasitic)
      .asJava

  private def asTurn(request: LlmRequest, text: String): LlmResponse =
    val sendTool = request.tools.find(_.name == "send_message")
    sendTool match
      case None => LlmResponse.ofText(text)
      case Some(tool) =>
        val trimmed = Option(text).map(_.trim).getOrElse("")
        if trimmed.isEmpty then LlmResponse("")
        else if trimmed == Signals.End then
          LlmResponse("", List(LlmToolCall("end_conversation", Json.obj())))
        else
          val conversation = tool.inputSchema.hcursor
            .downField("properties")
            .downField("conversation")
            .downField("enum")
            .downArray
            .as[String]
            .getOrElse(sys.error("send_message offers no conversation"))
          val args = Json.obj("conversation" -> conversation.asJson, "text" -> trimmed.asJson)
          LlmResponse("", List(LlmToolCall("send_message", args)))
