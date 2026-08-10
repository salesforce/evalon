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
        json.as[List[Json]].left.map(_.getMessage).map { list =>
          list.map { j =>
            j.asString match
              case Some(desc) => EvalCriterion(desc)
              case None =>
                val c = j.hcursor
                EvalCriterion(
                  description = c.downField("description").as[String].getOrElse(""),
                  weight = c.downField("weight").as[Double].getOrElse(1.0),
                )
          }
        }
