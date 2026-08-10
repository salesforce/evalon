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

class HotelBookingLookup(context: Json) extends Tool:
  private val bookings: Map[String, Json] =
    context.hcursor.downField("bookings").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "hotel_booking_lookup"
  override def description: String = "Look up a hotel booking by booking reference."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "booking_ref" -> Json.obj(
        "type" -> "string".asJson,
        "description" -> "The hotel booking reference".asJson,
      )
    ),
    "required" -> List("booking_ref").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val ref = arguments.get("booking_ref").flatMap(_.asString).getOrElse("")
    bookings.get(ref) match
      case Some(booking) => Future.successful(booking)
      case None =>
        Future.successful(Json.obj("error" -> s"Booking $ref not found".asJson))

class HotelCancel(context: Json) extends Tool:
  private val bookings: Map[String, Json] =
    context.hcursor.downField("bookings").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "hotel_cancel"
  override def description: String = "Cancel a hotel booking and process refund."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "booking_ref" -> Json.obj(
        "type" -> "string".asJson,
        "description" -> "The booking reference to cancel".asJson,
      ),
      "refund_type" -> Json.obj(
        "type" -> "string".asJson,
        "enum" -> List("full", "partial", "credit").asJson,
        "description" -> "Type of refund to process".asJson,
      ),
    ),
    "required" -> List("booking_ref", "refund_type").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val ref = arguments.get("booking_ref").flatMap(_.asString).getOrElse("")
    val refundType = arguments.get("refund_type").flatMap(_.asString).getOrElse("full")

    bookings.get(ref) match
      case None =>
        Future.successful(Json.obj("error" -> s"Booking $ref not found".asJson))
      case Some(booking) =>
        val total = booking.hcursor.downField("total").as[Double].getOrElse(0.0)
        val refundAmount = refundType match
          case "full"    => total
          case "partial" => total * 0.8
          case "credit"  => total
          case _         => 0.0
        val refundMethod =
          if refundType != "credit" then "corporate_card" else "account_credit"

        Future.successful(Json.obj(
          "status" -> "cancelled".asJson,
          "booking_ref" -> ref.asJson,
          "refund_type" -> refundType.asJson,
          "refund_amount" -> refundAmount.asJson,
          "refund_method" -> refundMethod.asJson,
          "message" -> f"Booking $ref cancelled. ${refundType.capitalize} refund of $$$refundAmount%.2f processed.".asJson,
        ))
