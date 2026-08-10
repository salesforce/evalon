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

package com.salesforce.spearhead.evalon.model

import io.circe.*
import io.circe.syntax.*

case class CriterionResult(
  criterion: EvalCriterion,
  passed: Boolean,
  reasoning: String,
  score: Double,
)

case class EvalResult(
  scenarioName: String,
  criterionResults: List[CriterionResult],
  overallScore: Double,
  summary: String,
)

object EvalResult:
  given Encoder[EvalResult] = Encoder.instance { r =>
    Json.obj(
      "scenario_name" -> r.scenarioName.asJson,
      "overall_score" -> r.overallScore.asJson,
      "summary" -> r.summary.asJson,
      "criteria" -> r.criterionResults.map { cr =>
        Json.obj(
          "description" -> cr.criterion.description.asJson,
          "passed" -> cr.passed.asJson,
          "score" -> cr.score.asJson,
          "reasoning" -> cr.reasoning.asJson,
        )
      }.asJson,
    )
  }
