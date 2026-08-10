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

import io.circe.*
import io.circe.syntax.*

import com.salesforce.spearhead.evalon.Settings
import com.salesforce.spearhead.evalon.llm.{AnthropicClient, ContentBlock, CreateMessageRequest}
import com.salesforce.spearhead.evalon.model.*
import com.salesforce.spearhead.evalon.tool.Tool

/** Sample agent powered by Claude with tool use. Stateless across `step` calls — the simulator
  * passes the conversation history each time. Tool-use rounds inside a single step accumulate
  * state locally.
  */
class ClaudeAgent(
  tools: Map[String, Tool],
  client: AnthropicClient,
  model: String = Settings.defaultModel,
  systemPrompt: String = "You are a helpful assistant.",
  agentName: String = "agent",
  maxToolRounds: Int = Settings.defaultMaxToolRounds,
)(using ec: ExecutionContext)
    extends Agent:

  private def toolDefinitions: List[Json] = tools.values.map { tool =>
    Json.obj(
      "name" -> tool.name.asJson,
      "description" -> tool.description.asJson,
      "input_schema" -> tool.parametersSchema,
    )
  }.toList

  override def step(
      history: List[HistoryEntry],
      events: List[Event],
      respondIn: String,
  ): Future[Action] =
    val baseMessages = history.map {
      case HistoryEntry.Turn(conversation, sender, content) =>
        Json.obj(
          "role" -> "user".asJson,
          "content" -> s"[$sender in $conversation]: $content".asJson,
        )
      case HistoryEntry.ToolUse(conversation, interaction) =>
        val resultText = interaction.result.error.getOrElse(interaction.result.result.noSpaces)
        val text =
          s"[you previously called tool '${interaction.call.toolName}' in $conversation with arguments " +
            s"${interaction.call.arguments.asJson.noSpaces} → result: $resultText]"
        Json.obj(
          "role" -> "user".asJson,
          "content" -> text.asJson,
        )
    }

    val eventLines =
      if events.isEmpty then ""
      else
        events.map { event =>
          s"[System event: ${event.name}] ${event.data.asJson.noSpaces}"
        }.mkString("\n") + "\n\n"

    val nudge =
      if baseMessages.isEmpty then s"""${eventLines}The "$respondIn" conversation is starting. Please begin."""
      else s"""${eventLines}[system]: Respond now in the "$respondIn" conversation."""

    val initialMessages = baseMessages :+ Json.obj(
      "role" -> "user".asJson,
      "content" -> nudge.asJson,
    )

    run(initialMessages)

  private def run(initialMessages: List[Json]): Future[Action] =
    var messages: List[Json] = initialMessages

    def loop(remainingRounds: Int, toolTrace: List[ToolInteraction]): Future[Action] =
      if remainingRounds <= 0 then
        Future.successful(Action.send(agentName, "", toolTrace))
      else
        val request = CreateMessageRequest(
          model = model,
          maxTokens = 1024,
          system = systemPrompt,
          messages = messages,
          tools = toolDefinitions,
        )
        client.createMessage(request).flatMap { response =>
          var assistantContent: List[Json] = Nil
          var toolUseBlocks: List[(String, ToolCall)] = Nil // (id, toolCall)
          var textContent: Option[String] = None

          response.content.foreach {
            case ContentBlock.TextBlock(text) =>
              assistantContent = assistantContent :+ Json.obj(
                "type" -> "text".asJson,
                "text" -> text.asJson,
              )
              val trimmed = text.trim
              // [NO_ACTION] only counts as a signal when it's the trailing token; strip it
              // from any visible text we surface to the runner.
              if trimmed == Signals.NoAction then ()
              else if trimmed.endsWith(Signals.NoAction) then
                textContent = Some(trimmed.stripSuffix(Signals.NoAction).trim)
              else textContent = Some(text)

            case ContentBlock.ToolUseBlock(id, name, input) =>
              assistantContent = assistantContent :+ Json.obj(
                "type" -> "tool_use".asJson,
                "id" -> id.asJson,
                "name" -> name.asJson,
                "input" -> input,
              )
              val arguments = input.as[Map[String, Json]].getOrElse(Map.empty)
              toolUseBlocks = toolUseBlocks :+ (id -> ToolCall(name, arguments))
          }

          messages = messages :+ Json.obj(
            "role" -> "assistant".asJson,
            "content" -> assistantContent.asJson,
          )

          if toolUseBlocks.isEmpty then
            val content = textContent.getOrElse("")
            Future.successful(Action.send(agentName, content, toolTrace))
          else
            val toolFutures = toolUseBlocks.map { (id, tc) =>
              tools.get(tc.toolName) match
                case None =>
                  val tr = ToolResult(tc.toolName, tc.arguments, Json.Null, Some(s"Tool '${tc.toolName}' not found"))
                  Future.successful((id, tc, tr))
                case Some(tool) =>
                  tool.execute(tc.arguments).map { output =>
                    (id, tc, ToolResult(tc.toolName, tc.arguments, output))
                  }.recover { case e: Exception =>
                    (id, tc, ToolResult(tc.toolName, tc.arguments, Json.Null, Some(e.getMessage)))
                  }
            }

            Future.sequence(toolFutures).flatMap { results =>
              val newInteractions = results.map { (_, tc, tr) => ToolInteraction(tc, tr) }
              val resultBlocks = results.map { (id, _, tr) =>
                val content = tr.error.getOrElse(tr.result.noSpaces)
                Json.obj(
                  "type" -> "tool_result".asJson,
                  "tool_use_id" -> id.asJson,
                  "content" -> content.asJson,
                )
              }
              messages = messages :+ Json.obj(
                "role" -> "user".asJson,
                "content" -> resultBlocks.asJson,
              )
              loop(remainingRounds - 1, toolTrace ++ newInteractions)
            }
        }

    loop(maxToolRounds, Nil)
