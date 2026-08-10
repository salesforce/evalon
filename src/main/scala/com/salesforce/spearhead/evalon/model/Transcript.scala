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

case class TranscriptEntry(
  conversation: Option[String] = None,
  message: Option[Message] = None,
  toolCall: Option[ToolCall] = None,
  toolResult: Option[ToolResult] = None,
  event: Option[Event] = None,
  durationMs: Option[Long] = None,
)

object TranscriptEntry:
  given Encoder[TranscriptEntry] = Encoder.instance { entry =>
    var fields: List[(String, Json)] = Nil

    entry.conversation.foreach(c => fields = ("conversation" -> c.asJson) :: fields)

    entry.message.foreach { m =>
      fields = ("type" -> "message".asJson) :: fields
      fields = ("sender" -> m.sender.asJson) :: fields
      fields = ("content" -> m.content.asJson) :: fields
    }

    entry.toolCall.foreach { tc =>
      fields = ("type" -> "tool_call".asJson) :: fields
      fields = ("tool" -> tc.toolName.asJson) :: fields
      fields = ("arguments" -> tc.arguments.asJson) :: fields
    }

    entry.toolResult.foreach { tr =>
      fields = ("type" -> "tool_result".asJson) :: fields
      fields = ("tool" -> tr.toolName.asJson) :: fields
      fields = ("arguments" -> tr.arguments.asJson) :: fields
      fields = ("result" -> tr.result) :: fields
      tr.error.foreach(e => fields = ("error" -> e.asJson) :: fields)
    }

    entry.event.foreach { e =>
      fields = ("type" -> "event".asJson) :: fields
      fields = ("name" -> e.name.asJson) :: fields
      fields = ("data" -> e.data.asJson) :: fields
    }

    entry.durationMs.foreach(d => fields = ("duration_ms" -> d.asJson) :: fields)

    Json.obj(fields.reverse*)
  }

/** Immutable transcript — the ScenarioRunner holds this as state and replaces it on each append.
  */
case class Transcript(entries: Vector[TranscriptEntry] = Vector.empty):

  def append(entry: TranscriptEntry): Transcript =
    copy(entries = entries :+ entry)

  def addMessage(
    sender: String,
    content: String,
    conversation: Option[String] = None,
    durationMs: Option[Long] = None,
  ): Transcript =
    append(
      TranscriptEntry(
        conversation = conversation,
        message = Some(Message(sender, content)),
        durationMs = durationMs,
      )
    )

  def addToolCall(toolCall: ToolCall, conversation: Option[String] = None): Transcript =
    append(TranscriptEntry(conversation = conversation, toolCall = Some(toolCall)))

  def addToolResult(toolResult: ToolResult, conversation: Option[String] = None): Transcript =
    append(TranscriptEntry(conversation = conversation, toolResult = Some(toolResult)))

  def addEvent(event: Event): Transcript =
    append(TranscriptEntry(event = Some(event)))

  def messages: Vector[Message] =
    entries.flatMap(_.message)

  def forConversations(names: Set[String]): Vector[TranscriptEntry] =
    entries.filter(e => e.conversation.isEmpty || names.exists(e.conversation.contains))

  /** Extract the ordered sequence of tool calls as a plan. */
  def toPlan: List[Json] =
    entries.flatMap(_.toolCall).zipWithIndex.map { case (tc, i) =>
      Json.obj(
        "step" -> (i + 1).asJson,
        "tool" -> tc.toolName.asJson,
        "arguments" -> tc.arguments.asJson,
      )
    }.toList

  def toJson: Json = entries.map(TranscriptEntry.given_Encoder_TranscriptEntry(_)).asJson
