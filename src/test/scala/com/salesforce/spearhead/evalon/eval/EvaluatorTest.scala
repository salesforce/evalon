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

package com.salesforce.spearhead.evalon.eval

import java.util.concurrent.{CompletableFuture, Executors}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.Await

import org.scalatest.funsuite.AnyFunSuite

import com.salesforce.spearhead.evalon.llm.{Llm, ChatMessage}
import com.salesforce.spearhead.evalon.model.*

class EvaluatorTest extends AnyFunSuite:

  test("judge parses JSON from Llm and scores weighted criteria") {
    val llm: Llm = (_, messages) =>
      val prompt = messages.map(_.content).mkString("\n")
      val text =
        if prompt.contains("confirmed with the user") then
          """{"passed": false, "score": 0.0, "reasoning": "missed confirmation"}"""
        else
          """{"passed": true, "score": 1.0, "reasoning": "rebooked"}"""
      CompletableFuture.completedFuture(text)

    val scenario = Scenario(
      name = "rebook",
      description = "rebook a flight",
      participants = Map.empty,
      conversations = Nil,
      evalCriteria = List(
        EvalCriterion(
          name = "rebooked",
          description = "rebooked the flight",
          criterionType = CriterionType.Binary,
          requireToolCall = true,
          weight = 2.0,
        ),
        EvalCriterion(
          name = "confirmed",
          description = "confirmed with the user",
          criterionType = CriterionType.Binary,
          requireToolCall = false,
          weight = 1.0,
        ),
      )
    )

    val result = Await.result(Evaluator(llm).evaluate(scenario, Transcript()), 5.seconds)
    assert(result.scenarioName == "rebook")
    assert(result.summary == "rebooked; missed confirmation")
    assert(result.criterionResults.head.passed)
    assert(!result.criterionResults(1).passed)
    assert(math.abs(result.overallScore - (1.0 * 2.0 + 0.0 * 1.0) / 3.0) < 1e-9)
    assert(result.criterionResults.size == 2)
  }

  test("completeAsync(system, messages) passes through to completeChat unchanged") {
    val received = scala.collection.mutable.ListBuffer.empty[(String, List[ChatMessage])]
    val llm: Llm = (system, messages) =>
      received += ((system, messages))
      CompletableFuture.completedFuture("structured")

    val text = Await.result(
      llm.completeAsync("sys", List(ChatMessage.user("hi"))),
      5.seconds
    )
    assert(text == "structured")
    assert(received.toList == List(("sys", List(ChatMessage.user("hi")))))
  }

  test("Llm.blocking wraps a synchronous chat function and hands it a java.util.List") {
    // The lambda is what a Java client writes: (system, messages) -> text, iterating messages as a
    // java.util.List. It owns any flattening; evalon does none.
    val llm = Llm.blocking { (system, messages) =>
      s"$system|${messages.size}|${messages.get(0).getContent}"
    }
    val text = Await.result(
      llm.completeAsync("sys", List(ChatMessage.user("hi"), ChatMessage.assistant("yo"))),
      5.seconds
    )
    assert(text == "sys|2|hi")
  }

  test("Llm.blocking runs the client on a caller-supplied executor, not the calling thread") {
    val exec = Executors.newSingleThreadExecutor { r =>
      val t = Thread(r, "test-blocking-pool")
      t.setDaemon(true)
      t
    }
    try
      val llm = Llm.blocking((_, _) => Thread.currentThread().getName, exec)
      val ranOn = Await.result(llm.completeAsync("s", List(ChatMessage.user("hi"))), 5.seconds)
      assert(ranOn == "test-blocking-pool")
    finally exec.shutdown()
  }

  test("non-empty evalPromptTemplate replaces the default judge system prompt") {
    var captured: String = null
    val llm: Llm = (_, messages) =>
      captured = messages.map(_.content).mkString("\n")
      CompletableFuture.completedFuture("""{"passed":true,"reasoning":"ok"}""")

    val scenario = Scenario(
      name = "custom-judge",
      description = "d",
      participants = Map.empty,
      conversations = Nil,
      evalCriteria = List(
        EvalCriterion(
          name = "ok",
          description = "ok",
          criterionType = CriterionType.Binary,
          requireToolCall = false,
        )
      ),
      evalPromptTemplate = Some("  Judge only tool use.  "),
    )
    Await.result(Evaluator(llm).evaluate(scenario, Transcript()), 5.seconds)
    assert(captured.startsWith("Judge only tool use."))
    assert(!captured.contains("You are an expert evaluator"))
  }

  test("empty evalPromptTemplate keeps the default judge system prompt") {
    var captured: String = null
    val llm: Llm = (_, messages) =>
      captured = messages.map(_.content).mkString("\n")
      CompletableFuture.completedFuture("""{"passed":true,"reasoning":"ok"}""")

    val scenario = Scenario(
      name = "default-judge",
      description = "d",
      participants = Map.empty,
      conversations = Nil,
      evalCriteria = List(
        EvalCriterion(
          name = "ok",
          description = "ok",
          criterionType = CriterionType.Binary,
          requireToolCall = false,
        )
      ),
      evalPromptTemplate = Some("   "),
    )
    Await.result(Evaluator(llm).evaluate(scenario, Transcript()), 5.seconds)
    assert(captured.startsWith("You are an expert evaluator"))
  }

  test("evaluate issues one LLM call per criterion") {
    val prompts = scala.collection.mutable.ListBuffer.empty[String]
    val llm: Llm = (_, messages) =>
      val prompt = messages.map(_.content).mkString("\n")
      prompts += prompt
      val json =
        if prompt.contains("first") then """{"passed":true,"reasoning":"a"}"""
        else """{"passed":false,"reasoning":"b"}"""
      CompletableFuture.completedFuture(json)

    val scenario = Scenario(
      name = "two",
      description = "d",
      participants = Map.empty,
      conversations = Nil,
      evalCriteria = List(
        EvalCriterion("c1", "first", CriterionType.Binary, requireToolCall = false),
        EvalCriterion("c2", "second", CriterionType.Binary, requireToolCall = false),
      ),
    )
    val result = Await.result(Evaluator(llm).evaluate(scenario, Transcript()), 5.seconds)
    assert(prompts.size == 2)
    assert(prompts.exists(_.contains("first")))
    assert(prompts.exists(_.contains("second")))
    assert(result.criterionResults.map(_.passed) == List(true, false))
  }

  test("passThreshold overrides LLM passed using score") {
    val llm: Llm = (_, _) =>
      CompletableFuture.completedFuture("""{"passed": true, "score": 0.4, "reasoning": "weak"}""")
    val scenario = Scenario(
      name = "threshold",
      description = "d",
      participants = Map.empty,
      conversations = Nil,
      evalCriteria = List(
        EvalCriterion(
          name = "quality",
          description = "quality",
          criterionType = CriterionType.Scored,
          requireToolCall = false,
          passThreshold = Some(0.7),
        )
      ),
    )
    val result = Await.result(Evaluator(llm).evaluate(scenario, Transcript()), 5.seconds)
    assert(!result.criterionResults.head.passed)
    assert(result.criterionResults.head.score == 0.4)
  }
