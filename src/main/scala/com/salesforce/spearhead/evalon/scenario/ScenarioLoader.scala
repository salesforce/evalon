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

package com.salesforce.spearhead.evalon.scenario

import java.nio.file.{Files, Path}

import io.circe.*
import io.circe.yaml.parser

import com.salesforce.spearhead.evalon.model.*

object ScenarioLoader:

  private val reservedNames = Set("user", "assistant")

  def load(path: Path): Either[String, Scenario] =
    val yamlString = Files.readString(path)
    for
      json <- parser.parse(yamlString).left.map(_.getMessage)
      scenario <- decodeScenario(json)
    yield scenario

  private def decodeScenario(json: Json): Either[String, Scenario] =
    val c = json.hcursor
    for
      name <- c.downField("name").as[String].left.map(_.getMessage)
      description <- c.downField("description").as[String].left.map(_.getMessage)
      participants <- decodeParticipants(c.downField("participants"))
      conversations <- decodeConversations(c.downField("conversations"))
      observations <- decodeObservations(c.downField("observations"))
      eventSources <- decodeEventSources(c.downField("event_sources"))
      context = c.downField("context").focus.getOrElse(Json.obj())
      evalCriteria <- decodeEvalCriteria(c.downField("eval_criteria"))
      evalPromptTemplate = c.downField("eval_prompt_template").as[String].toOption
      maxTurns = c.downField("max_turns").as[Int].getOrElse(20)
    yield Scenario(
      name = name,
      description = description,
      participants = participants,
      conversations = conversations,
      observations = observations,
      eventSources = eventSources,
      context = context,
      evalCriteria = evalCriteria,
      evalPromptTemplate = evalPromptTemplate,
      maxTurns = maxTurns,
    )

  private def decodeParticipants(cursor: ACursor): Either[String, Map[String, ParticipantConfig]] =
    cursor.focus match
      case None => Right(Map.empty)
      case Some(json) =>
        json.as[Map[String, Json]].left.map(_.getMessage).flatMap { map =>
          val results = map.map { (name, pJson) =>
            if reservedNames.contains(name) then
              Left(
                s"Participant name '$name' is reserved (conflicts with LLM API roles). Choose a different name."
              )
            else
              val c = pJson.hcursor
              val typeStr = c.downField("type").as[String].getOrElse("simulated")
              val pType = typeStr match
                case "evaluated" => ParticipantType.Evaluated
                case "custom"    => ParticipantType.Custom
                case _           => ParticipantType.Simulated
              val responseSpeed = c.downField("responseSpeed").as[String].toOption.flatMap {
                case "fast"   => Some(ResponseSpeed.Fast)
                case "medium" => Some(ResponseSpeed.Medium)
                case "slow"   => Some(ResponseSpeed.Slow)
                case _        => None
              }
              Right(
                name -> ParticipantConfig(
                  name = name,
                  participantType = pType,
                  persona = c.downField("persona").as[String].getOrElse(""),
                  goal = c.downField("goal").as[String].getOrElse(""),
                  contextFacts = c.downField("context_facts").focus.getOrElse(Json.obj()),
                  endpoint = c.downField("endpoint").as[String].toOption,
                  responseSpeed = responseSpeed,
                )
              )
          }
          results.foldLeft(Right(Map.empty[String, ParticipantConfig]): Either[String, Map[String, ParticipantConfig]]) {
            case (Left(e), _)              => Left(e)
            case (_, Left(e))              => Left(e)
            case (Right(acc), Right(pair)) => Right(acc + pair)
          }
        }

  private def decodeConversations(cursor: ACursor): Either[String, List[ConversationConfig]] =
    cursor.focus match
      case None => Right(Nil)
      case Some(json) =>
        json.as[List[Json]].left.map(_.getMessage).map { list =>
          list.map { j =>
            val c = j.hcursor
            ConversationConfig(
              name = c.downField("name").as[String].getOrElse(""),
              between = c.downField("between").as[List[String]].getOrElse(Nil),
              initiatedBy = c.downField("initiated_by").as[String].toOption,
            )
          }
        }

  private def decodeObservations(cursor: ACursor): Either[String, List[ObservationConfig]] =
    cursor.focus match
      case None => Right(Nil)
      case Some(json) =>
        json.as[List[Json]].left.map(_.getMessage).map { list =>
          list.map { j =>
            val c = j.hcursor
            ObservationConfig(
              participant = c.downField("participant").as[String].getOrElse(""),
              observes = c.downField("observes").as[List[String]].getOrElse(Nil),
            )
          }
        }

  private def decodeEventSources(cursor: ACursor): Either[String, List[EventSourceConfig]] =
    cursor.focus match
      case None => Right(Nil)
      case Some(json) =>
        json.as[Map[String, Json]].left.map(_.getMessage).map { map =>
          map.map { (name, esJson) =>
            val c = esJson.hcursor
            val emits = c.downField("emits").as[List[Json]].getOrElse(Nil).map { ej =>
              val ec = ej.hcursor
              EventEmitConfig(
                event = ec.downField("event").as[String].getOrElse(""),
                schema = ec.downField("schema").as[Map[String, String]].getOrElse(Map.empty),
              )
            }
            EventSourceConfig(
              name = name,
              sourceType = c.downField("type").as[String].getOrElse("simulated"),
              description = c.downField("description").as[String].getOrElse(""),
              emits = emits,
              className = c.downField("class_name").as[String].toOption,
            )
          }.toList
        }

  private def decodeEvalCriteria(cursor: ACursor): Either[String, List[EvalCriterion]] =
    cursor.focus match
      case None => Right(Nil)
      case Some(json) =>
        json.as[List[Json]].left.map(_.getMessage).flatMap { list =>
          list.foldLeft(Right(Nil): Either[String, List[EvalCriterion]]) { (acc, j) =>
            acc.flatMap(cs => decodeEvalCriterion(j).map(cs :+ _))
          }
        }

  private def decodeEvalCriterion(json: Json): Either[String, EvalCriterion] =
    json.asString match
      case Some(desc) =>
        Right(
          EvalCriterion(
            name = desc,
            description = desc,
            criterionType = CriterionType.Binary,
            requireToolCall = false
          )
        )
      case None =>
        val c = json.hcursor
        for
          criterionType <- decodeCriterionType(c)
        yield EvalCriterion(
          name = c.downField("name").as[String].toOption.getOrElse(""),
          description = c.downField("description").as[String].toOption.getOrElse(""),
          criterionType = criterionType,
          requireToolCall = c.downField("require_tool_call").as[Boolean].toOption.getOrElse(false),
          passThreshold = c.downField("pass_threshold").as[Double].toOption,
          tags = c.downField("tags").as[List[String]].toOption.getOrElse(Nil),
          weight = c.downField("weight").as[Double].toOption.getOrElse(1.0),
        )

  private def decodeCriterionType(c: HCursor): Either[String, CriterionType] =
    c.downField("criterion_type").as[String].toOption match
      case None => Right(CriterionType.Binary)
      case Some(s) =>
        s.trim.toLowerCase match
          case "binary"  => Right(CriterionType.Binary)
          case "scored"  => Right(CriterionType.Scored)
          case "ordinal" => Right(CriterionType.Ordinal)
          case other =>
            Left(s"Unknown criterion type '$other' (expected binary, scored, or ordinal)")
