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

package com.salesforce.spearhead.evalon.output

import java.nio.file.{Files, Path}
import java.time.{Instant, ZoneOffset}
import java.time.format.DateTimeFormatter

import io.circe.{Encoder, Json}
import io.circe.syntax.*

import com.salesforce.spearhead.evalon.model.{EvalResult, Scenario, Transcript}

object DatasetWriter:

  def saveScenarioMetadata(scenario: Scenario, scenarioDir: Path): Unit =
    Files.createDirectories(scenarioDir)
    val metaPath = scenarioDir.resolve("scenario.json")
    val meta = Json.obj(
      "scenario_name" -> scenario.name.asJson,
      "scenario_description" -> scenario.description.asJson,
    )
    Files.writeString(metaPath, meta.spaces2)

  def saveDatasetEntry(
    scenario: Scenario,
    transcript: Transcript,
    evalResult: EvalResult,
    outputDir: Path,
  ): Path =
    val timestamp = DateTimeFormatter
      .ofPattern("yyyyMMdd'T'HHmmss'Z'")
      .withZone(ZoneOffset.UTC)
      .format(Instant.now())

    val scenarioDir = outputDir.resolve(scenario.name)
    Files.createDirectories(scenarioDir)

    val filename = s"${scenario.name}_$timestamp.json"
    val outputPath = scenarioDir.resolve(filename)

    val entry = Json.obj(
      "plan" -> transcript.toPlan.asJson,
      "transcript" -> transcript.toJson,
      "evaluation" -> EvalResult.given_Encoder_EvalResult(evalResult),
    )

    Files.writeString(outputPath, entry.spaces2)
    outputPath
