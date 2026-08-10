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
import io.circe.syntax.*

/** Signal constants used in prompts and checked by the simulator/agent. Scenario YAML prompts must
  * use these exact strings.
  */
object Signals:
  val NoQuestion = "[NO_QUESTION]"
  val NoAction = "[NO_ACTION]"
  val End = "[END]"

case class Message(sender: String, content: String)

object Message:
  given Encoder[Message] = Encoder.instance { m =>
    Json.obj("sender" -> m.sender.asJson, "content" -> m.content.asJson)
  }
  given Decoder[Message] = Decoder.instance { c =>
    for
      sender <- c.downField("sender").as[String]
      content <- c.downField("content").as[String]
    yield Message(sender, content)
  }

case class ToolCall(toolName: String, arguments: Map[String, Json])

object ToolCall:
  given Encoder[ToolCall] = Encoder.instance { tc =>
    Json.obj("tool_name" -> tc.toolName.asJson, "arguments" -> tc.arguments.asJson)
  }
  given Decoder[ToolCall] = Decoder.instance { c =>
    for
      toolName <- c.downField("tool_name").as[String]
      arguments <- c.downField("arguments").as[Map[String, Json]]
    yield ToolCall(toolName, arguments)
  }

case class ToolResult(
  toolName: String,
  arguments: Map[String, Json],
  result: Json,
  error: Option[String] = None,
)

object ToolResult:
  given Encoder[ToolResult] = Encoder.instance { tr =>
    val base = Json.obj(
      "tool_name" -> tr.toolName.asJson,
      "arguments" -> tr.arguments.asJson,
      "result" -> tr.result,
    )
    tr.error.fold(base)(e => base.deepMerge(Json.obj("error" -> e.asJson)))
  }
  given Decoder[ToolResult] = Decoder.instance { c =>
    for
      toolName <- c.downField("tool_name").as[String]
      arguments <- c.downField("arguments").as[Map[String, Json]]
      result <- c.downField("result").as[Json]
      error <- c.downField("error").as[Option[String]]
    yield ToolResult(toolName, arguments, result, error)
  }

case class Event(name: String, data: Map[String, Json] = Map.empty)

object Event:
  given Encoder[Event] = Encoder.instance { e =>
    Json.obj("name" -> e.name.asJson, "data" -> e.data.asJson)
  }
  given Decoder[Event] = Decoder.instance { c =>
    for
      name <- c.downField("name").as[String]
      data <- c.downField("data").as[Option[Map[String, Json]]]
    yield Event(name, data.getOrElse(Map.empty))
  }

case class ToolInteraction(call: ToolCall, result: ToolResult)

object ToolInteraction:
  given Encoder[ToolInteraction] = Encoder.instance { ti =>
    Json.obj("call" -> ti.call.asJson, "result" -> ti.result.asJson)
  }
  given Decoder[ToolInteraction] = Decoder.instance { c =>
    for
      call <- c.downField("call").as[ToolCall]
      result <- c.downField("result").as[ToolResult]
    yield ToolInteraction(call, result)
  }

/** An action taken by a participant (sent back to the scenario runner). */
enum Action:
  case Send(message: Message, toolTrace: List[ToolInteraction] = Nil)
  case End

object Action:
  def send(sender: String, content: String, toolTrace: List[ToolInteraction] = Nil): Action =
    Action.Send(Message(sender, content), toolTrace)

  given Encoder[Action] = Encoder.instance {
    case Action.Send(message, toolTrace) =>
      Json.obj(
        "type" -> "send".asJson,
        "message" -> message.asJson,
        "tool_trace" -> toolTrace.asJson,
      )
    case Action.End =>
      Json.obj("type" -> "end".asJson)
  }
  given Decoder[Action] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "send" =>
        for
          message <- c.downField("message").as[Message]
          toolTrace <- c.downField("tool_trace").as[Option[List[ToolInteraction]]]
        yield Action.Send(message, toolTrace.getOrElse(Nil))
      case "end" =>
        Right(Action.End)
      case other =>
        Left(DecodingFailure(s"Unknown action type: $other", c.history))
    }
  }
