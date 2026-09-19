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

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}

import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

import com.salesforce.spearhead.evalon.llm.*
import com.salesforce.spearhead.evalon.model.*

class ClaudeAgentTest extends AnyFunSuite:

  private given ExecutionContext = ExecutionContext.global

  /** A client that always requests a (nonexistent) tool, so the agent's loop never reaches a final
    * text answer and keeps recursing until maxToolRounds is exhausted.
    */
  private class ToolLoopingClient
      extends AnthropicClient("http://unused", "unused", backend = null):
    override def createMessage(request: CreateMessageRequest): Future[CreateMessageResponse] =
      Future.successful(
        CreateMessageResponse(
          id = "resp",
          content = List(ContentBlock.ToolUseBlock("call-1", "noop", Json.obj())),
          stopReason = "tool_use",
          usage = Usage(0, 0)
        )
      )

  test("exhausting maxToolRounds returns a visible message, not an empty send") {
    val agent = ClaudeAgent(
      tools = Map.empty,
      client = ToolLoopingClient(),
      maxToolRounds = 2
    )

    val action =
      Await.result(agent.step(history = Nil, events = Nil, respondIn = "chat"), 5.seconds)

    val expected = "Reached the maximum of 2 tool-use rounds without completing the request."
    action match
      case Action.Send(message, toolTrace) =>
        assert(message.sender == "agent")
        assert(message.content == expected) // non-empty, so the runner won't drop it
        assert(toolTrace.size == 2) // one (tool-not-found) interaction per round before exhaustion
      case other =>
        fail(s"expected Action.Send, got $other")
  }
