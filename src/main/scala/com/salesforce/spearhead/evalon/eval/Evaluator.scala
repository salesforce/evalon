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
4. One evaluation criterion

Determine if the agent met the criterion. Provide:
- "passed": true/false
- "score": 0.0 to 1.0 (how well it was met)
- "reasoning": brief explanation

Respond with valid JSON only, in this format:
{"passed": true, "score": 0.9, "reasoning": "..."}"""

  private val toolCallVerificationPrompt: String =
    """This criterion REQUIRES a tool/action invocation. To pass:
   - The transcript MUST show evidence of the expected action in the turn trace
   - Look for tool calls, function invocations, or action records in the trace
   - If no matching action appears in trace, the criterion does NOT pass — but write the gap per the reasoning above (why it was not called), not just "the action is missing"
"""

  def evaluate(scenario: Scenario, transcript: Transcript): Future[EvalResult] =
    val contextText =
      if scenario.context == Json.obj() then "None"
      else scenario.context.spaces2

    val transcriptText = formatTranscript(transcript)

    val judged = Future.traverse(scenario.evalCriteria) { criterion =>
      val criterionPrompt = s"""## Evaluation Criterion
${criterion.description}
${criterion.criterionType match
    case CriterionType.Binary => "This criterion is binary (passed/failed)."
    case CriterionType.Scored => "This criterion is scored (0.0 to 1.0)."
    case CriterionType.Rubric =>
      """This criterion is a rubric. Its levels are defined in the criterion text above.
Pick the one level that best matches the transcript."""
}
"""
      val toolSection =
        if criterion.requireToolCall then s"$toolCallVerificationPrompt\n" else ""
      val rubricPassedField =
        if criterion.requireToolCall then
          "- passed: true if the required tool call is found, false if it is not.\n"
        else ""
      val outputFormatPrompt = criterion.criterionType match
        case CriterionType.Binary =>
          """# Response Format

Return valid JSON only, with these fields:
- passed: boolean
- reasoning: string (brief explanation)

Do not add any other fields. Do not wrap in markdown."""
        case CriterionType.Rubric =>
          s"""# Response Format

Return valid JSON only, with these fields:
- score: number. Use the score written next to the chosen level in the criterion, exactly as written.
  Only return a score that appears in the criterion's levels. Do not return a value between levels.
  Do not rescale it to 0 to 1.
  If the levels have no scores, give the chosen level a score from 0 (lowest level) to 1 (highest level).
${rubricPassedField}- reasoning: string (brief explanation)

Do not add any other fields. Do not wrap in markdown."""
        case _ =>
          """# Response Format

Return valid JSON only, with these fields:
- score: number from 0 to 1
- reasoning: string (brief explanation)

Do not add any other fields. Do not wrap in markdown."""

      val fullPrompt = scenario.evalPromptTemplate.map(_.trim).filter(_.nonEmpty) match
        case Some(prompt) =>
          s"$prompt\n\n$transcriptText\n\n$criterionPrompt$toolSection$outputFormatPrompt"
        case None =>
          s"""$judgeSystemPrompt\n\n## Scenario
Name: ${scenario.name}
Description: ${scenario.description}
Conversations: ${scenario.conversations.map(_.name).mkString(", ")}

## Ground Truth
$contextText

## Transcript
$transcriptText

$criterionPrompt
Evaluate the agent's performance against this criterion. Use the ground truth data to verify factual correctness of the agent's responses.

$outputFormatPrompt"""

      Future {
        llm.completeAsync("", List(ChatMessage.user(fullPrompt)))
      }.flatten.map(parseCriterionResult(criterion, _))
    }

    judged.map { criterionResults =>
      val totalWeight = scenario.evalCriteria.map(_.weight).sum
      val overallScore =
        if totalWeight > 0 then
          criterionResults.map(cr => cr.score * cr.criterion.weight).sum / totalWeight
        else 0.0
      val overallPassed = 
        if criterionResults.isEmpty then 
          false // no criteria to evaluate, so we can't say if the overall result is passed
        else criterionResults.forall(_.passed)
      
      EvalResult(
        scenarioName = scenario.name,
        criterionResults = criterionResults,
        overallScore = overallScore,
        overallPassed = overallPassed,
        summary = criterionResults.map(_.reasoning).filter(_.nonEmpty).mkString("; "),
      )
    }

  private def parseCriterionResult(criterion: EvalCriterion, response: String): CriterionResult =
    val text = Option(response).filter(_.nonEmpty).getOrElse("{}")
    // (?s) enables dotall mode so .* matches across newlines
    val jsonStr = """(?s)```(?:json)?\s*(.*?)\s*```""".r
      .findFirstMatchIn(text)
      .map(_.group(1))
      .getOrElse(text)
    val raw = parse(jsonStr).getOrElse(Json.obj())
    val c = raw.hcursor
    val reasoning = c.downField("reasoning").as[String].getOrElse("")
    val (passed, score) = criterion.criterionType match
      case CriterionType.Binary =>
        val passed = c.downField("passed").as[Boolean].getOrElse(false)
        (passed, if passed then 1.0 else 0.0)
      case CriterionType.Rubric =>
        return parseRubricResult(criterion, c, reasoning, Option(response).getOrElse(""))
      case _ =>
        val score = c.downField("score").as[Double].getOrElse(0.0)
        val passed = criterion.passThreshold match
          case Some(threshold) => score >= threshold
          case None => c.downField("passed").as[Boolean].getOrElse(false)
        (passed, score)
    CriterionResult(
      criterion = criterion,
      passed = passed,
      reasoning = reasoning,
      score = score,
    )

  /** Rubric `passed` is `score >= passThreshold`. The judge's own `passed` is read only when a tool
    * call is required: anything but `true` means the call was not found. An invalid score fails the
    * criterion with an explicit reason.
    */
  private def parseRubricResult(
      criterion: EvalCriterion,
      c: HCursor,
      reasoning: String,
      response: String,
  ): CriterionResult =
    // circe decodes a JSON null as NaN.
    val score = c.downField("score").as[Double].toOption.filter(_.isFinite)
    val toolCallMissing =
      criterion.requireToolCall && !c.downField("passed").as[Boolean].toOption.contains(true)
    (score, criterion.passThreshold) match
      case _ if Option(criterion.description).forall(_.trim.isEmpty) =>
        CriterionResult(
          criterion = criterion,
          passed = false,
          reasoning = "Rubric criterion has no description with its levels.",
          score = 0.0,
        )
      case _ if toolCallMissing =>
        CriterionResult(
          criterion = criterion,
          passed = false,
          reasoning = if reasoning.trim.nonEmpty then reasoning else "Required tool call not found.",
          score = score.getOrElse(0.0),
        )
      case (None, _) =>
        val detail = if reasoning.trim.nonEmpty then reasoning else response.take(200)
        CriterionResult(
          criterion = criterion,
          passed = false,
          reasoning = s"Invalid judge response: score missing or not a number. $detail".trim,
          score = 0.0,
        )
      case (Some(s), Some(threshold)) =>
        CriterionResult(
          criterion = criterion,
          passed = s >= threshold,
          reasoning = reasoning,
          score = s,
        )
      case (Some(s), None) =>
        CriterionResult(
          criterion = criterion,
          passed = false,
          reasoning = s"Rubric criterion has no passThreshold. $reasoning".trim,
          score = s,
        )

  private def formatTranscript(transcript: Transcript): String =
    transcript.entries.map { entry =>
      val conv = entry.conversation.map(c => s" ($c)").getOrElse("")
      entry.message.map { m =>
        val line = s"[${m.sender}]$conv: ${m.content}"
        m.trace.fold(line)(j => s"$line\n[trace] ${j.noSpaces}")
      }
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
