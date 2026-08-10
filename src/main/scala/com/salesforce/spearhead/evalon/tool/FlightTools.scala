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

package com.salesforce.spearhead.evalon.tool

import scala.concurrent.Future

import io.circe.*
import io.circe.syntax.*

class FlightLookup(context: Json) extends Tool:
  private val flights: Map[String, Json] =
    context.hcursor.downField("flights").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "flight_lookup"

  override def description: String =
    "Look up flight information by flight number or search for available flights on a route."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "flight_number" -> Json.obj(
        "type" -> "string".asJson,
        "description" -> "Flight number to look up".asJson,
      ),
      "origin" -> Json.obj(
        "type" -> "string".asJson,
        "description" -> "Origin airport code".asJson,
      ),
      "destination" -> Json.obj(
        "type" -> "string".asJson,
        "description" -> "Destination airport code".asJson,
      ),
    ),
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val flightNumber = arguments.get("flight_number").flatMap(_.asString)
    flightNumber match
      case Some(fn) =>
        flights.get(fn) match
          case Some(flight) => Future.successful(flight)
          case None =>
            Future.successful(Json.obj("error" -> s"Flight $fn not found".asJson))
      case None =>
        val origin = arguments.get("origin").flatMap(_.asString)
        val destination = arguments.get("destination").flatMap(_.asString)
        val results = flights.values.filter { f =>
          val c = f.hcursor
          val matchOrigin = origin.forall(o => c.downField("origin").as[String].contains(o))
          val matchDest = destination.forall(d => c.downField("destination").as[String].contains(d))
          val onTime = c.downField("status").as[String].contains("on_time")
          matchOrigin && matchDest && onTime
        }.toList
        Future.successful(Json.obj("available_flights" -> results.asJson))

class FlightRebook(context: Json) extends Tool:
  private val flights: Map[String, Json] =
    context.hcursor.downField("flights").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "flight_rebook"

  override def description: String = "Rebook a passenger from one flight to another."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "booking_ref" -> Json.obj(
        "type" -> "string".asJson,
        "description" -> "Original booking reference".asJson,
      ),
      "new_flight_number" -> Json.obj(
        "type" -> "string".asJson,
        "description" -> "New flight to rebook to".asJson,
      ),
    ),
    "required" -> List("booking_ref", "new_flight_number").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val bookingRef = arguments.get("booking_ref").flatMap(_.asString).getOrElse("")
    val newFlight = arguments.get("new_flight_number").flatMap(_.asString).getOrElse("")

    flights.get(newFlight) match
      case None =>
        Future.successful(Json.obj("error" -> s"Flight $newFlight not found".asJson))
      case Some(flightData) =>
        Future.successful(Json.obj(
          "status" -> "confirmed".asJson,
          "booking_ref" -> bookingRef.asJson,
          "new_flight" -> flightData,
          "message" -> s"Successfully rebooked to $newFlight".asJson,
        ))
