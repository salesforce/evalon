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

package com.salesforce.spearhead.evalon

import java.net.http.HttpClient
import java.nio.file.{Path, Paths}

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext}

import com.salesforce.spearhead.evalon.agent.{Agent, ClaudeAgent, RemoteAgent}
import com.salesforce.spearhead.evalon.llm.AnthropicClient
import com.salesforce.spearhead.evalon.model.{ParticipantType, Signals}
import com.salesforce.spearhead.evalon.output.{DatasetWriter, TranscriptPrinter}
import com.salesforce.spearhead.evalon.scenario.ScenarioLoader
import com.salesforce.spearhead.evalon.tool.ToolRegistry

object Main:

  private val directSystemPrompt: String =
    """You are a helpful customer support agent. You assist customers by looking up information, taking actions on their behalf, and communicating clearly and empathetically.

Use the available tools to help resolve the customer's issue. Always confirm important actions with the customer before and after performing them."""

  private val assistSystemPrompt: String =
    s"""You are an AI assistant helping a human support representative in real time. You are NOT talking to the customer directly — the representative handles that.

Your role:
- Proactively look up relevant information using tools when you hear the customer's request
- Provide concise guidance and suggestions to the representative
- Perform tool actions (lookups, cancellations, rebookings) when appropriate
- Suggest what the representative should say or do next
- Flag any policy details or important information the representative should know

Keep your messages brief and actionable. The representative will see your messages and use them to respond to the customer. The representative may also ask you questions for clarification — answer them directly and concisely.

After the representative responds to the customer, you may observe their response. If you notice an error or have additional follow-up, provide it. Otherwise, stay silent.

If you have no useful information, recommendations, or actions to contribute, respond with exactly: ${Signals.NoAction}"""

  def main(args: Array[String]): Unit =
    val scenarioPath =
      args.headOption.map(Path.of(_)).getOrElse(Path.of("scenarios/reschedule_flight.yaml"))

    val scenario = ScenarioLoader.load(scenarioPath) match
      case Right(s) => s
      case Left(e) => throw RuntimeException(s"Failed to load scenario: $e")

    println(s"Running scenario: ${scenario.name}")
    println(s"Conversations: ${scenario.conversations.map(_.name)}")
    println(s"Criteria: ${scenario.evalCriteria.size}")
    println()

    given ExecutionContext = ExecutionContext.global

    val client = AnthropicClient.create()
    val agentFacts = scenario.participants.collectFirst {
      case (_, p) if p.participantType == ParticipantType.Evaluated => p.contextFacts
    }.getOrElse(scenario.context)
    val tools = ToolRegistry.build(agentFacts)
    val hasObservers = scenario.observations.nonEmpty
    val systemPrompt = if hasObservers then assistSystemPrompt else directSystemPrompt

    val evaluatedEndpoint = scenario.participants.collectFirst {
      case (_, p) if p.participantType == ParticipantType.Evaluated && p.endpoint.isDefined =>
        p.endpoint.get
    }

    val agent: Agent = evaluatedEndpoint match
      case Some(url) =>
        // Force HTTP/1.1 to avoid upgrade requests that uvicorn rejects
        val httpClient = HttpClient.newBuilder()
          .version(HttpClient.Version.HTTP_1_1)
          .build()
        val backend = sttp.client4.httpclient.HttpClientFutureBackend.usingClient(httpClient)
        println(s"Using remote agent at $url")
        RemoteAgent(url, backend)
      case None =>
        ClaudeAgent(tools, client, systemPrompt = systemPrompt)

    val showConv = scenario.conversations.size > 1
    val printer = TranscriptPrinter(showConversation = showConv)
    val options = EvalonRunOptions().withOnEntryFn(printer.apply)

    println("=== Transcript ===\n")
    val system = EvalonRunner.newSystem()
    try
      val result = EvalonRunner(system).run(scenario, agent, client, options)
      TranscriptPrinter.printEval(result.scalaEvalResult)

      val outputDir = Paths.get("target", "evalon-runs")
      val savedPath =
        DatasetWriter.saveDatasetEntry(scenario, result.scalaTranscript, result.scalaEvalResult, outputDir)
      println(s"\n✓ Transcript saved to: $savedPath")
    finally
      system.terminate()
      Await.ready(system.whenTerminated, 30.seconds)
