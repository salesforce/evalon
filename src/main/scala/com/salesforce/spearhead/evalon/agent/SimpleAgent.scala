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

import java.util
import java.util.concurrent.CompletionStage

import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.*
import scala.jdk.FutureConverters.*
import scala.util.{Failure, Success, Try}

import io.circe.syntax.*

import com.salesforce.spearhead.evalon.model.{Action, Event, HistoryEntry}

/** One conversational turn from the simulator history, without Scala collections or circe. */
final class HistoryTurn(
    private val conversation: String,
    private val sender: String,
    private val content: String
):
  def getConversation: String = conversation
  def getSender: String = sender
  def getContent: String = content

/** Reply from a {@link SimpleAgent} step. */
final class AgentReply(private val content: String, private val end: Boolean):
  def getContent: String = content
  def isEnd: Boolean = end

object AgentReply:
  def send(content: String): AgentReply = AgentReply(content, false)
  def end(): AgentReply = AgentReply("", true)

/** Simplified evaluated agent: history turns + optional event lines */
// TODO: add tool trace
@FunctionalInterface
trait SimpleAgent:
  def step(
      respondIn: String,
      history: util.List[HistoryTurn],
      events: util.List[String]
  ): CompletionStage[AgentReply]

object SimpleAgent:
  def toAgent(simpleAgent: SimpleAgent, agentName: String)(using ExecutionContext): Agent =
    new Agent:
      def step(
          history: List[HistoryEntry],
          events: List[Event],
          respondIn: String
      ): Future[Action] =
        val turns = history
          .collect { case HistoryEntry.Turn(c, s, content) => HistoryTurn(c, s, content) }
          .asJava
        val eventLines = events.map(e => s"${e.name}: ${e.data.asJson.noSpaces}").asJava
        Try(simpleAgent.step(respondIn, turns, eventLines)) match
          case Success(null) =>
            Future.failed(
              NullPointerException("SimpleAgent.step must return a non-null CompletionStage")
            )
          case Success(stage) =>
            stage.asScala.map { reply =>
              if reply == null || reply.isEnd then Action.End
              else Action.send(agentName, Option(reply.getContent).getOrElse(""))
            }
          case Failure(e) => Future.failed(e)
