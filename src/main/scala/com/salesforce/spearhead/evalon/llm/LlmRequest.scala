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

import scala.jdk.CollectionConverters.*

import io.circe.Json
import io.circe.parser.parse

/**
 * A tool the model may call. `inputSchema` is a JSON Schema object describing the arguments. Java:
 * {@code getInputSchemaJson()} returns it as a string to pass to your client's tool API.
 */
final case class LlmTool(name: String, description: String, inputSchema: Json):
  def getName: String = name
  def getDescription: String = description
  def getInputSchemaJson: String = inputSchema.noSpaces

/** A tool call the model made: the tool's name and its arguments as a JSON object. */
final case class LlmToolCall(name: String, arguments: Json):
  def getName: String = name
  def getArgumentsJson: String = arguments.noSpaces

object LlmToolCall:
  /** Java: build a call from the arguments JSON your client returned. Throws if it isn't JSON. */
  def of(name: String, argumentsJson: String): LlmToolCall =
    parse(argumentsJson) match
      case Right(json) => LlmToolCall(name, json)
      case Left(err) =>
        throw IllegalArgumentException(s"Tool call '$name' arguments are not JSON: ${err.message}")

/**
 * One model call: a system prompt, roled turns, and the tools the model may call (empty for a plain
 * text call, such as the judge's).
 */
final case class LlmRequest(
    system: String,
    messages: List[ChatMessage],
    tools: List[LlmTool] = Nil
):
  def getSystem: String = system
  def getMessages: java.util.List[ChatMessage] = messages.asJava
  def getTools: java.util.List[LlmTool] = tools.asJava

/** The model's reply: its text (empty if none) and the tool calls it made, in order. */
final case class LlmResponse(text: String, toolCalls: List[LlmToolCall] = Nil):
  def getText: String = text
  def getToolCalls: java.util.List[LlmToolCall] = toolCalls.asJava

object LlmResponse:
  /** A reply with text and no tool calls. */
  def ofText(text: String): LlmResponse = LlmResponse(Option(text).getOrElse(""))

  /** Java: a reply with text and tool calls. Either may be null, meaning none. */
  def of(text: String, toolCalls: java.util.List[LlmToolCall]): LlmResponse =
    LlmResponse(
      Option(text).getOrElse(""),
      Option(toolCalls).map(_.asScala.toList).getOrElse(Nil)
    )
