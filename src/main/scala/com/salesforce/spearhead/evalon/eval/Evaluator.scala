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

import scala.concurrent.{ExecutionContext, Future}

import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*

import com.salesforce.spearhead.evalon.llm.{Llm, ChatMessage}
import com.salesforce.spearhead.evalon.model.*

/** LLM-as-judge evaluator that scores agent performance against criteria. */
class Evaluator(llm: Llm)(using ec: ExecutionContext):

  private val judgeSystemPrompt: String =
    """You are an expert evaluator judging an AI agent's performance in a simulated scenario.

You will be given:
1. A scenario description
2. Ground truth data (the actual data the tools operate on)
3. A full transcript of what happened
4. A list of evaluation criteria

For each criterion, determine if the agent met it. Provide:
- "passed": true/false
- "score": 0.0 to 1.0 (how well it was met)
- "reasoning": brief explanation

Respond with valid JSON only, in this format:
{
  "criteria": [
    {"passed": true, "score": 0.9, "reasoning": "..."},
    ...
  ],
  "summary": "Overall assessment of the agent's performance"
}"""

  def evaluate(scenario: Scenario, transcript: Transcript): Future[EvalResult] =
    val criteriaText = scenario.evalCriteria.zipWithIndex.map { (c, i) =>
      s"${i + 1}. ${c.description} (weight: ${c.weight})"
    }.mkString("\n")

    val contextText =
      if scenario.context == Json.obj() then "None"
      else scenario.context.spaces2

    val transcriptText = formatTranscript(transcript)

    val userPrompt = s"""## Scenario
Name: ${scenario.name}
Description: ${scenario.description}
Conversations: ${scenario.conversations.map(_.name).mkString(", ")}

## Ground Truth
$contextText

## Transcript
$transcriptText

## Evaluation Criteria
$criteriaText

Evaluate the agent's performance against each criterion. Use the ground truth data to verify factual correctness of the agent's responses."""

    llm.completeAsync(judgeSystemPrompt, List(ChatMessage.user(userPrompt))).map { response =>
      val text = Option(response).filter(_.nonEmpty).getOrElse("{}")

      // Extract JSON from response (model may wrap in markdown fences)
      // (?s) enables dotall mode so .* matches across newlines
      val jsonStr = """(?s)```(?:json)?\s*(.*?)\s*```""".r
        .findFirstMatchIn(text)
        .map(_.group(1))
        .getOrElse(text)

      val raw = parse(jsonStr).flatMap(_.as[Json]).getOrElse(Json.obj())
      val criteriaResults = raw.hcursor.downField("criteria").as[List[Json]].getOrElse(Nil)

      val criterionResults = scenario.evalCriteria.zipWithIndex.map { (criterion, i) =>
        val r = criteriaResults.lift(i).getOrElse(Json.obj())
        val c = r.hcursor
        CriterionResult(
          criterion = criterion,
          passed = c.downField("passed").as[Boolean].getOrElse(false),
          reasoning = c.downField("reasoning").as[String].getOrElse(""),
          score = c.downField("score").as[Double].getOrElse(0.0),
        )
      }

      val totalWeight = scenario.evalCriteria.map(_.weight).sum
      val overallScore =
        if totalWeight > 0 then
          criterionResults.map(cr => cr.score * cr.criterion.weight).sum / totalWeight
        else 0.0

      val summary = raw.hcursor.downField("summary").as[String].getOrElse("")

      EvalResult(
        scenarioName = scenario.name,
        criterionResults = criterionResults,
        overallScore = overallScore,
        summary = summary,
      )
    }

  private def formatTranscript(transcript: Transcript): String =
    transcript.entries.map { entry =>
      val conv = entry.conversation.map(c => s" ($c)").getOrElse("")
      entry.message.map { m => s"[${m.sender}]$conv: ${m.content}" }
        .orElse(entry.toolCall.map { tc =>
          s"[tool_call] ${tc.toolName}(${tc.arguments.asJson.noSpaces})"
        })
        .orElse(entry.toolResult.map { tr =>
          tr.error match
            case Some(e) => s"[tool_error] $e"
            case None    => s"[tool_result] ${tr.result.noSpaces}"
        })
        .orElse(entry.event.map { e =>
          s"[event] ${e.name}: ${e.data.asJson.noSpaces}"
        })
        .getOrElse("")
    }.mkString("\n")
