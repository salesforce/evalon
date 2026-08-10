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

import io.circe.*
import io.circe.syntax.*

case class CreateMessageRequest(
  model: String,
  maxTokens: Int,
  system: String,
  messages: List[Json],
  tools: List[Json] = Nil,
)

object CreateMessageRequest:
  given Encoder[CreateMessageRequest] = Encoder.instance { r =>
    val base = Json.obj(
      "model" -> r.model.asJson,
      "max_tokens" -> r.maxTokens.asJson,
      "system" -> r.system.asJson,
      "messages" -> r.messages.asJson,
    )
    if r.tools.nonEmpty then base.deepMerge(Json.obj("tools" -> r.tools.asJson))
    else base
  }

case class Usage(inputTokens: Int, outputTokens: Int)

object Usage:
  given Decoder[Usage] = Decoder.instance { c =>
    for
      input <- c.downField("input_tokens").as[Int]
      output <- c.downField("output_tokens").as[Int]
    yield Usage(input, output)
  }

enum ContentBlock:
  case TextBlock(text: String)
  case ToolUseBlock(id: String, name: String, input: Json)

object ContentBlock:
  given Decoder[ContentBlock] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "text" =>
        c.downField("text").as[String].map(ContentBlock.TextBlock(_))
      case "tool_use" =>
        for
          id <- c.downField("id").as[String]
          name <- c.downField("name").as[String]
          input <- c.downField("input").as[Json]
        yield ContentBlock.ToolUseBlock(id, name, input)
      case other =>
        Left(DecodingFailure(s"Unknown content block type: $other", c.history))
    }
  }

case class CreateMessageResponse(
  id: String,
  content: List[ContentBlock],
  stopReason: String,
  usage: Usage,
)

object CreateMessageResponse:
  given Decoder[CreateMessageResponse] = Decoder.instance { c =>
    for
      id <- c.downField("id").as[String]
      content <- c.downField("content").as[List[ContentBlock]]
      stopReason <- c.downField("stop_reason").as[String]
      usage <- c.downField("usage").as[Usage]
    yield CreateMessageResponse(id, content, stopReason, usage)
  }
