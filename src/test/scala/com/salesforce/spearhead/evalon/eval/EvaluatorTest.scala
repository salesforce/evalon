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

import java.util.concurrent.CompletableFuture

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.Await

import org.scalatest.funsuite.AnyFunSuite

import com.salesforce.spearhead.evalon.llm.{Llm, ChatMessage}
import com.salesforce.spearhead.evalon.model.*

class EvaluatorTest extends AnyFunSuite:

  test("judge parses JSON from Llm and scores weighted criteria") {
    val llm: Llm = _ =>
      CompletableFuture.completedFuture("""{
        "criteria": [
          {"passed": true, "score": 1.0, "reasoning": "rebooked"},
          {"passed": false, "score": 0.0, "reasoning": "missed confirmation"}
        ],
        "summary": "partial"
      }""")

    val scenario = Scenario(
      name = "rebook",
      description = "rebook a flight",
      participants = Map.empty,
      conversations = Nil,
      evalCriteria = List(
        EvalCriterion("rebooked the flight", 2.0),
        EvalCriterion("confirmed with the user", 1.0)
      )
    )

    val result = Await.result(Evaluator(llm).evaluate(scenario, Transcript()), 5.seconds)
    assert(result.scenarioName == "rebook")
    assert(result.summary == "partial")
    assert(result.criterionResults.head.passed)
    assert(!result.criterionResults(1).passed)
    assert(math.abs(result.overallScore - (1.0 * 2.0 + 0.0 * 1.0) / 3.0) < 1e-9)
  }

  test("Llm.blocking wraps a synchronous function") {
    val llm = Llm.blocking(identity)
    val text = Await.result(llm.completeAsync("hello"), 5.seconds)
    assert(text == "hello")
  }

  test("toPrompt flattens system and turns into one string") {
    val prompt = Llm.toPrompt("You are a judge.", List(ChatMessage.user("score this")))
    assert(prompt == "You are a judge.\n\nuser: score this")
  }

  test("default completeChat flattens to complete") {
    val llm: Llm = prompt => CompletableFuture.completedFuture(prompt)
    val text = Await.result(
      llm.completeAsync("You are a judge.", List(ChatMessage.user("score this"))),
      5.seconds
    )
    assert(text == "You are a judge.\n\nuser: score this")
  }

  test("completeAsync(system, messages) uses completeChat, not flattened complete") {
    val received = scala.collection.mutable.ListBuffer.empty[(String, List[ChatMessage])]
    val llm = new Llm:
      def complete(prompt: String) =
        CompletableFuture.completedFuture(s"flat:$prompt")
      override def completeChat(system: String, messages: List[ChatMessage]) =
        received += ((system, messages))
        CompletableFuture.completedFuture("structured")

    val text = Await.result(
      llm.completeAsync("sys", List(ChatMessage.user("hi"))),
      5.seconds
    )
    assert(text == "structured")
    assert(received.toList == List(("sys", List(ChatMessage.user("hi")))))
  }
