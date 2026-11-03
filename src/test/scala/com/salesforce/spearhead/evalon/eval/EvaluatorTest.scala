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

  private val rubricLadder =
    """How clearly did the agent explain the refund policy?
      |Levels (pick exactly one):
      |- absent (0.0): did not explain the policy
      |- generic (0.5): correct but generic explanation
      |- clear (0.75): clear, plain-language explanation
      |- clear-with-example (1.0): clear, with a concrete example""".stripMargin

  private def rubricCriterion(
      passThreshold: Option[Double] = Some(0.75),
      requireToolCall: Boolean = false,
  ): EvalCriterion =
    EvalCriterion(
      name = "clarity",
      description = rubricLadder,
      criterionType = CriterionType.Rubric,
      requireToolCall = requireToolCall,
      passThreshold = passThreshold,
    )

  private def judgeOnce(
      criterion: EvalCriterion,
      reply: String,
      template: Option[String] = None,
  ): (CriterionResult, String) =
    var captured: String = null
    val llm: Llm = (_, messages) =>
      captured = messages.map(_.content).mkString("\n")
      CompletableFuture.completedFuture(reply)
    val scenario = Scenario(
      name = "rubric",
      description = "d",
      participants = Map.empty,
      conversations = Nil,
      evalCriteria = List(criterion),
      evalPromptTemplate = template,
    )
    val result = Await.result(Evaluator(llm).evaluate(scenario, Transcript()), 5.seconds)
    (result.criterionResults.head, captured)

  private def rubricResult(reply: String, passThreshold: Option[Double] = Some(0.75)) =
    judgeOnce(rubricCriterion(passThreshold), reply)._1

  test("rubric passes at the threshold and keeps the judge reasoning") {
    val r = rubricResult("""{"score":0.75,"reasoning":"Plain language."}""")
    assert(r.passed)
    assert(r.score == 0.75)
    assert(r.reasoning == "Plain language.")
  }

  test("rubric fails below the threshold") {
    val r = rubricResult("""{"score":0.5,"reasoning":"Generic."}""")
    assert(!r.passed)
    assert(r.score == 0.5)
  }

  test("rubric accepts a numeric string score") {
    val r = rubricResult("""{"score":"0.75","reasoning":"ok"}""")
    assert(r.passed)
    assert(r.score == 0.75)
  }

  test("rubric compares scores and threshold on the caller's scale") {
    val r = rubricResult("""{"score":4,"reasoning":"Clear."}""", Some(3.0))
    assert(r.passed)
    assert(r.score == 4.0)
    assert(!rubricResult("""{"score":2,"reasoning":"Vague."}""", Some(3.0)).passed)
    assert(rubricResult("""{"score":-1,"reasoning":"r"}""", Some(-2.0)).passed)
  }

  test("rubric rejects a missing, null, non-numeric or infinite score") {
    val replies = List(
      """{"reasoning":"r"}""",
      """{"score":null,"reasoning":"r"}""",
      """{"score":"high","reasoning":"r"}""",
      """{"score":1e400,"reasoning":"r"}""",
      """{"score":-1e400,"reasoning":"r"}""",
    )
    replies.foreach { reply =>
      val r = rubricResult(reply, Some(-10.0))
      assert(!r.passed, reply)
      assert(r.score == 0.0, reply)
      assert(r.reasoning == "Invalid judge response: score missing or not a number. r", reply)
    }
  }

  test("rubric reports the start of the raw response when the reply is not JSON") {
    val r = rubricResult("I think the agent was clear.")
    assert(!r.passed)
    assert(
      r.reasoning ==
        "Invalid judge response: score missing or not a number. I think the agent was clear."
    )
  }

  test("rubric reports only the first 200 characters of a long non-JSON reply") {
    val r = rubricResult("x" * 300)
    assert(!r.passed)
    assert(r.reasoning == s"Invalid judge response: score missing or not a number. ${"x" * 200}")
  }

  test("rubric without a required tool call ignores the judge's passed field") {
    assert(!rubricResult("""{"passed":true,"score":0.5,"reasoning":"r"}""").passed)
    assert(rubricResult("""{"passed":false,"score":0.75,"reasoning":"r"}""").passed)
  }

  private def toolCallRubricResult(reply: String, passThreshold: Option[Double] = Some(0.75)) =
    judgeOnce(rubricCriterion(passThreshold, requireToolCall = true), reply)._1

  test("rubric requiring a tool call fails unless passed is true, at any threshold") {
    for
      threshold <- List(Some(0.75), Some(0.0), Some(-5.0), None)
      passed <- List(""""passed":false,""", "", """"passed":"yes",""")
    do
      val r = toolCallRubricResult(s"""{$passed"score":0.75,"reasoning":"No lookup."}""", threshold)
      assert(!r.passed, (threshold, passed))
      assert(r.reasoning == "No lookup.", (threshold, passed))
    assert(!toolCallRubricResult("""{"score":1.0,"reasoning":"r"}""", Some(0.0)).passed)
  }

  test("rubric missing a required tool call explains itself when the judge gives no reasoning") {
    val r = toolCallRubricResult("""{"score":1.0}""")
    assert(!r.passed)
    assert(r.reasoning == "Required tool call not found.")
  }

  test("rubric missing a required tool call keeps the judge's score") {
    assert(toolCallRubricResult("""{"passed":false,"reasoning":"r"}""").score == 0.0)
    val scored = toolCallRubricResult("""{"passed":false,"score":1.0,"reasoning":"r"}""")
    assert(!scored.passed)
    assert(scored.score == 1.0)
  }

  test("rubric requiring a tool call passes by threshold when passed is true") {
    assert(toolCallRubricResult("""{"passed":true,"score":0.75,"reasoning":"r"}""").passed)
    assert(!toolCallRubricResult("""{"passed":true,"score":0.5,"reasoning":"r"}""").passed)
  }

  test("rubric with a blank description fails with an explicit reason") {
    List("", "  ", null).foreach { description =>
      val criterion = rubricCriterion().copy(description = description)
      val (r, _) = judgeOnce(criterion, """{"score":1.0,"reasoning":"ok"}""")
      assert(!r.passed, s"[$description]")
      assert(r.score == 0.0, s"[$description]")
      assert(
        r.reasoning == "Rubric criterion has no description with its levels.",
        s"[$description]",
      )
    }
  }

  test("rubric without passThreshold fails with an explicit reason") {
    val r = rubricResult("""{"score":0.75,"reasoning":"ok"}""", None)
    assert(!r.passed)
    assert(r.score == 0.75)
    assert(r.reasoning == "Rubric criterion has no passThreshold. ok")
  }

  test("rubric prompt asks for the score of one level on both prompt paths") {
    val reply = """{"score":0.75,"reasoning":"ok"}"""
    List(None, Some("Judge the transcript.")).foreach { template =>
      val (_, prompt) = judgeOnce(rubricCriterion(), reply, template)
      assert(prompt.contains(rubricLadder))
      assert(prompt.contains("This criterion is a rubric. Its levels are defined in the criterion"))
      assert(prompt.contains("Pick the one level that best matches the transcript."))
      assert(prompt.contains("Use the score written next to the chosen level in the criterion, exactly as written."))
      assert(prompt.contains("Only return a score that appears in the criterion's levels."))
      assert(prompt.contains("Do not return a value between levels.\n  Do not rescale it to 0 to 1."))
      assert(
        prompt.contains(
          "If the levels have no scores, give the chosen level a score from 0 (lowest level) to 1 (highest level)."
        )
      )
      assert(prompt.contains("Do not add any other fields. Do not wrap in markdown."))
      assert(!prompt.contains("- passed:"))
    }
  }

  test("rubric prompt requiring a tool call asks whether the tool call was found") {
    val reply = """{"passed":false,"reasoning":"no call"}"""
    List(None, Some("Judge the transcript.")).foreach { template =>
      val (_, prompt) = judgeOnce(rubricCriterion(requireToolCall = true), reply, template)
      assert(
        prompt.contains(
          "highest level).\n" +
            "- passed: true if the required tool call is found, false if it is not.\n" +
            "- reasoning: string (brief explanation)"
        )
      )
    }
  }

  test("scored prompt keeps the score-only response format") {
    val scored = EvalCriterion(
      name = "tone",
      description = "tone",
      criterionType = CriterionType.Scored,
      requireToolCall = true,
      passThreshold = Some(0.7),
    )
    val (_, prompt) =
      judgeOnce(scored, """{"score":0.8,"reasoning":"ok"}""", Some("Judge the transcript."))
    assert(prompt.contains("This criterion is scored (0.0 to 1.0)."))
    assert(prompt.contains("not just \"the action is missing\"\n\n# Response Format"))
    assert(prompt.contains("- score: number from 0 to 1\n- reasoning: string (brief explanation)"))
  }
