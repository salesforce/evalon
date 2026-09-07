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

import java.util

import scala.jdk.CollectionConverters.*

import com.salesforce.spearhead.evalon.model.{EvalResult, Transcript}

final class EvalonCriterionResult(
    private val description: String,
    private val passed: Boolean,
    private val score: Double,
    private val reasoning: String
):
  def getDescription: String = description
  def isPassed: Boolean = passed
  def getScore: Double = score
  def getReasoning: String = reasoning

/** Java-friendly evaluation outcome. Avoids Scala lists / circe / {@code Option}. */
final class EvalonResult(
    private val scenarioName: String,
    private val overallScore: Double,
    private val summary: String,
    private val criterionResults: util.List[EvalonCriterionResult],
    private[evalon] val scalaEvalResult: EvalResult,
    private[evalon] val scalaTranscript: Transcript
):
  def getScenarioName: String = scenarioName
  def getOverallScore: Double = overallScore
  def getSummary: String = summary
  def getCriterionResults: util.List[EvalonCriterionResult] = criterionResults

object EvalonResult:
  def from(evalResult: EvalResult, transcript: Transcript): EvalonResult =
    val criteria = evalResult.criterionResults.map { cr =>
      EvalonCriterionResult(
        cr.criterion.description,
        cr.passed,
        cr.score,
        cr.reasoning
      )
    }.asJava
    EvalonResult(
      evalResult.scenarioName,
      evalResult.overallScore,
      evalResult.summary,
      criteria,
      evalResult,
      transcript
    )
