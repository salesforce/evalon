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

/** A single entry in the simulation history passed to an agent.
  *
  * Either a conversational turn (someone said something) or a tool interaction the agent
  * itself performed in a prior step. Recording past tool work prevents the agent from
  * re-executing actions across steps.
  */
enum HistoryEntry:
  case Turn(conversation: String, sender: String, content: String)
  case ToolUse(conversation: String, interaction: ToolInteraction)

object HistoryEntry:
  given Encoder[HistoryEntry] = Encoder.instance {
    case HistoryEntry.Turn(conversation, sender, content) =>
      Json.obj(
        "type" -> "turn".asJson,
        "conversation" -> conversation.asJson,
        "sender" -> sender.asJson,
        "content" -> content.asJson,
      )
    case HistoryEntry.ToolUse(conversation, interaction) =>
      Json.obj(
        "type" -> "tool_use".asJson,
        "conversation" -> conversation.asJson,
        "interaction" -> interaction.asJson,
      )
  }
  given Decoder[HistoryEntry] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "turn" =>
        for
          conversation <- c.downField("conversation").as[String]
          sender <- c.downField("sender").as[String]
          content <- c.downField("content").as[String]
        yield HistoryEntry.Turn(conversation, sender, content)
      case "tool_use" =>
        for
          conversation <- c.downField("conversation").as[String]
          interaction <- c.downField("interaction").as[ToolInteraction]
        yield HistoryEntry.ToolUse(conversation, interaction)
      case other =>
        Left(DecodingFailure(s"Unknown history entry type: $other", c.history))
    }
  }
