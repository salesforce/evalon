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

import io.circe.Json
import io.circe.syntax.*

/** One chat turn. Java: {@code new ChatMessage("user", "...")} or {@code ChatMessage.user("...")}. */
final case class ChatMessage(role: String, content: String):
  def getRole: String = role
  def getContent: String = content
  def toJson: Json = Json.obj("role" -> role.asJson, "content" -> content.asJson)

object ChatMessage:
  def user(content: String): ChatMessage = ChatMessage("user", content)
  def assistant(content: String): ChatMessage = ChatMessage("assistant", content)
  def of(role: String, content: String): ChatMessage = ChatMessage(role, content)
