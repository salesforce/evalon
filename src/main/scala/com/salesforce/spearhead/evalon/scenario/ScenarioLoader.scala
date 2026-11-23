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

  /**
   * The participant under test and the one conversation it replies in. Every check on the evaluated
   * participant lives here, so the runner and the validation can't disagree about who it is.
   */
  case class Evaluated(name: String, conversation: String)

  /** The scenario's evaluated participant, or why it doesn't have exactly one that can run. */
  def evaluated(scenario: Scenario): Either[String, Evaluated] =
    val names = scenario.participants
      .collect {
        case (name, p) if p.participantType == ParticipantType.Evaluated => name
      }
      .toList
      .sorted
    names match
      case Nil =>
        Left(
          "Scenario has no evaluated participant, so there is nothing to test. " +
            "Give the agent under test `type: evaluated`."
        )
      case name :: Nil =>
        // Observed conversations don't count: the agent reads them but can't post there.
        scenario.conversations.filter(_.between.contains(name)).map(_.name) match
          case conversation :: Nil => Right(Evaluated(name, conversation))
          case Nil =>
            Left(
              s"Evaluated participant $name is in no conversation, so it can never reply. " +
                "Add it to one conversation's `between` (observing a conversation doesn't count)."
            )
          case several =>
            // The agent can't choose where to post: it replies where it was addressed, and a
            // step woken by an observed event would reply in an arbitrary one of them.
            Left(
              s"Evaluated participant $name is in ${several.size} conversations " +
                s"(${several.sorted.mkString(", ")}), but an evaluated participant can reply in " +
                "only one. Keep one and observe the others; " +
                "see https://github.com/salesforce/evalon/issues/33."
            )
      case several =>
        // The runner would run every evaluated participant as one: one name, one conversation,
        // one shared Agent. Reject the scenario rather than misattribute its messages.
        Left(
          s"Scenario has ${several.size} evaluated participants (${several.mkString(", ")}), " +
            "but only one evaluated participant is supported. Make the others simulated."
        )

  /**
   * Checks a scenario for configurations that can't be simulated, or that crash or misclassify
   * failures at runtime, returning the evaluated participant. Run by `ScenarioRunner`, which
   * simulates without judging.
   */
  def validateSimulation(scenario: Scenario): Either[String, Evaluated] =
    for
      _ <- validateEventSourceNames(scenario)
      _ <- validateEmittedEventNames(scenario)
      _ <- validateConversationNames(scenario)
      _ <- validateConversationMembers(scenario)
      _ <- validateInitiators(scenario)
      _ <- validateObservations(scenario)
      agent <- evaluated(scenario)
    yield agent

  /**
   * [[validateSimulation]], plus the checks for judging the run. Run by {@link #load} and again by
   * {@code EvalonRunner} before a simulation starts, so scenarios built directly in code are
   * covered too.
   */
  def validateScenario(scenario: Scenario): Either[String, Unit] =
    for
      _ <- validateSimulation(scenario)
      _ <- validateEvalCriteria(scenario)
    yield ()

  private def validateConversationNames(scenario: Scenario): Either[String, Unit] =
    // The runner routes by conversation name: a message is delivered to every participant in a
    // conversation of that name. Two conversations with one name merge, so a message reaches
    // participants of the other conversation too. A missing name decodes as "", which collides the
    // same way. An event source with a conversation's name makes the name ambiguous as an
    // observation target.
    val names = scenario.conversations.map(_.name)
    val unnamed = names.count(n => n == null || n.trim.isEmpty)
    val repeated = names.filter(n => n != null && n.trim.nonEmpty).groupBy(identity).collect {
      case (name, uses) if uses.size > 1 => name
    }
    val sharedWithSources = names.toSet.intersect(scenario.eventSources.map(_.name).toSet)
    if unnamed > 0 then
      Left(s"$unnamed conversation(s) have no name. Every conversation needs a unique name.")
    else if repeated.nonEmpty then
      Left(
        s"Conversation name(s) used more than once: ${repeated.toList.sorted.mkString(", ")}. " +
          "Messages are routed by name, so conversations with one name would merge."
      )
    else if sharedWithSources.nonEmpty then
      Left(
        s"Name(s) used by both a conversation and an event source: " +
          s"${sharedWithSources.toList.sorted.mkString(", ")}. An observation of that name would " +
          "be ambiguous."
      )
    else Right(())

  private def validateConversationMembers(scenario: Scenario): Either[String, Unit] =
    // The runner spawns only declared participants, so an undeclared name in `between` (usually a
    // typo) is nobody: nothing is delivered to it, and a kickoff for it never starts the
    // conversation. Checking `between` also covers an initiator with the same typo.
    val unknown = for
      conv <- scenario.conversations
      member <- conv.between.distinct
      if !scenario.participants.contains(member)
    yield s"${conv.name}: $member"
    if unknown.nonEmpty then
      Left(
        s"Conversation member(s) that are not participants: ${unknown.mkString(", ")}. " +
          s"Declared participants: ${scenario.participants.keys.toList.sorted.mkString(", ")}."
      )
    else Right(())

  private def validateObservations(scenario: Scenario): Either[String, Unit] =
    // An observation with an unknown observer, an unknown target, or no target does nothing:
    // the runner looks each name up and skips misses. In an assist scenario the agent would never
    // see the chat it assists with, and the run would still be judged. A target is a conversation
    // or an event source, whose emitted events observers receive.
    val targets = scenario.conversations.map(_.name).toSet ++ scenario.eventSources.map(_.name)
    val problems = scenario.observations.flatMap { obs =>
      val observer =
        if scenario.participants.contains(obs.participant) then Nil
        else List(s"observer ${obs.participant} is not a participant")
      val observed =
        if obs.observes.isEmpty then List(s"${obs.participant} observes nothing")
        else
          obs.observes.distinct.filterNot(targets).map { target =>
            s"${obs.participant} observes $target, which is not a conversation or event source"
          }
      observer ++ observed
    }
    if problems.nonEmpty then
      Left(
        s"Observation(s) that would do nothing: ${problems.mkString("; ")}. " +
          "Check the names against `participants`, `conversations` and `event_sources`, and " +
          "that each observation lists what it observes under `observes`."
      )
    else Right(())

  private def validateInitiators(scenario: Scenario): Either[String, Unit] =
    // The runner sends the kickoff to the initiator, wherever it is. One outside the conversation
    // would open a conversation it isn't in (an evaluated agent replies where it was addressed).
    // Members are already checked to be participants, so an initiator in `between` is one too.
    val outside = for
      conv <- scenario.conversations
      initiator <- conv.initiatedBy
      if !conv.between.contains(initiator)
    yield s"${conv.name} is initiated by $initiator, who is not in its between list"
    if outside.nonEmpty then
      Left(
        s"Conversation initiator(s) outside their conversation: ${outside.mkString("; ")}. " +
          "Add the initiator to `between`, or initiate with a participant who is in it."
      )
    else Right(())

  private def validateEvalCriteria(scenario: Scenario): Either[String, Unit] =
    // The judge would make no calls and score the run 0.0, failed: an incomplete scenario that
    // looks like a failing agent.
    if scenario.evalCriteria.isEmpty then
      Left(
        "Scenario has no eval_criteria, so every run would score 0 and fail. " +
          "Add at least one criterion."
      )
    else
      // The description is what the judge applies; an empty one (`[{}]`, `[""]`) still counts
      // toward the list, so check each. Numbered from 1, since a blank criterion may have no name.
      val blank = scenario.evalCriteria.zipWithIndex.collect {
        case (c, i) if EvalCriterion.isBlank(c.description) =>
          val name = if EvalCriterion.isBlank(c.name) then "" else s" (${c.name.trim})"
          s"criterion ${i + 1}$name"
      }
      if blank.nonEmpty then
        Left(
          (blank match
            case one :: Nil => s"Eval $one has no description. "
            case several => s"Eval criteria have no description: ${several.mkString(", ")}. "
          ) + "The description is what the judge checks the transcript against."
        )
      else Right(())

  private def validateEventSourceNames(scenario: Scenario): Either[String, Unit] =
    val participantNames = scenario.participants.keySet
    val eventSourceNames = scenario.eventSources.map(_.name).toSet
    val collisions = participantNames.intersect(eventSourceNames)
    if collisions.nonEmpty then
      Left(
        s"Event source name(s) collide with participant name(s): ${collisions.mkString(", ")}. " +
          "Event sources and participants must have distinct names to avoid failure misclassification."
      )
    else Right(())

  private def validateEmittedEventNames(scenario: Scenario): Either[String, Unit] =
    val reserved = for
      source <- scenario.eventSources
      emit <- source.emits
      if Event.ReservedNames.contains(emit.event)
    yield s"${source.name}: ${emit.event}"
    if reserved.nonEmpty then
      Left(
        s"Event source(s) declare reserved event name(s): ${reserved.mkString(", ")}. " +
          s"${Event.ReservedNames.toList.sorted.mkString(", ")} are sent by the harness itself."
      )
    else Right(())

  def load(path: Path): Either[String, Scenario] =
    val yamlString = Files.readString(path)
    for
      json <- parser.parse(yamlString).left.map(_.getMessage)
      scenario <- decodeScenario(json)
      _ <- validateScenario(scenario)
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
      maxTurns = maxTurns
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
                case "custom" => ParticipantType.Custom
                case _ => ParticipantType.Simulated
              val responseSpeed = c.downField("response_speed").as[String].toOption.flatMap {
                case "fast" => Some(ResponseSpeed.Fast)
                case "medium" => Some(ResponseSpeed.Medium)
                case "slow" => Some(ResponseSpeed.Slow)
                case _ => None
              }
              Right(
                name -> ParticipantConfig(
                  name = name,
                  participantType = pType,
                  persona = c.downField("persona").as[String].getOrElse(""),
                  goal = c.downField("goal").as[String].getOrElse(""),
                  template = c.downField("template").as[String].toOption,
                  endpoint = c.downField("endpoint").as[String].toOption,
                  responseSpeed = responseSpeed
                )
              )
          }
          results.foldLeft(
            Right(Map.empty[String, ParticipantConfig]): Either[
              String,
              Map[String, ParticipantConfig]
            ]
          ) {
            case (Left(e), _) => Left(e)
            case (_, Left(e)) => Left(e)
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
              initiatedBy = c.downField("initiated_by").as[String].toOption
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
              observes = c.downField("observes").as[List[String]].getOrElse(Nil)
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
                schema = ec.downField("schema").as[Map[String, String]].getOrElse(Map.empty)
              )
            }
            EventSourceConfig(
              name = name,
              sourceType = c.downField("type").as[String].getOrElse("simulated"),
              description = c.downField("description").as[String].getOrElse(""),
              emits = emits,
              className = c.downField("class_name").as[String].toOption
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
        for criterionType <- decodeCriterionType(c)
        yield EvalCriterion(
          name = c.downField("name").as[String].toOption.getOrElse(""),
          description = c.downField("description").as[String].toOption.getOrElse(""),
          criterionType = criterionType,
          requireToolCall = c.downField("require_tool_call").as[Boolean].toOption.getOrElse(false),
          passThreshold = c.downField("pass_threshold").as[Double].toOption,
          weight = c.downField("weight").as[Double].toOption.getOrElse(1.0)
        )

  private def decodeCriterionType(c: HCursor): Either[String, CriterionType] =
    c.downField("criterion_type").as[String].toOption match
      case None => Right(CriterionType.Binary)
      case Some(s) =>
        s.trim.toLowerCase match
          case "binary" => Right(CriterionType.Binary)
          case "scored" => Right(CriterionType.Scored)
          case "rubric" => Right(CriterionType.Rubric)
          case other =>
            Left(s"Unknown criterion type '$other' (expected binary, scored, or rubric)")
