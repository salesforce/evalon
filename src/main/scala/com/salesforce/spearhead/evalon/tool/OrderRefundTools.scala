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

class VerifyCustomerContactIdentity(context: Json) extends Tool:
  private val contacts: Map[String, Json] =
    context.hcursor.downField("contacts").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "verify_customer_contact_identity"
  override def description: String =
    "Verifies the identity of the customer contact by matching the provided email and name against the contact record."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "email" -> Json.obj("type" -> "string".asJson, "description" -> "Customer email address to verify.".asJson),
      "name" -> Json.obj("type" -> "string".asJson, "description" -> "Customer name to verify.".asJson),
    ),
    "required" -> List("email", "name").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val email = arguments.get("email").flatMap(_.asString).getOrElse("")
    val name = arguments.get("name").flatMap(_.asString).getOrElse("")
    contacts.get(email) match
      case None => Future.successful(Json.obj("verification_status" -> "Unverified".asJson))
      case Some(c) =>
        val storedName = c.hcursor.downField("name").as[String].getOrElse("")
        val status = if storedName.equalsIgnoreCase(name) then "Verified" else "Unverified"
        Future.successful(Json.obj("verification_status" -> status.asJson))

class RetrieveCustomerOrderInformation(context: Json) extends Tool:
  private val orders: Map[String, Json] =
    context.hcursor.downField("orders").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "retrieve_customer_order_information"
  override def description: String =
    "Provides detailed information about a customer order, including orderId, description, and total price."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "order_number" -> Json.obj("type" -> "string".asJson, "description" -> "The customer order number to retrieve.".asJson),
    ),
    "required" -> List("order_number").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val orderNumber = arguments.get("order_number").flatMap(_.asString).getOrElse("")
    orders.get(orderNumber) match
      case None =>
        Future.successful(Json.obj("order_info" -> "Error: No order found with the provided Order Number".asJson))
      case Some(o) =>
        val c = o.hcursor
        val info = s"""Order Details:
Order Name: ${c.downField("name").as[String].getOrElse("N/A")}
Order Number: ${c.downField("order_number").as[String].getOrElse("N/A")}
Description: ${c.downField("description").as[String].getOrElse("No description available")}
Total Price: $$${c.downField("total_price").as[Double].getOrElse(0.0)}"""
        Future.successful(Json.obj("order_info" -> info.asJson))

class VerifyCustomerRefundEligibility(context: Json) extends Tool:
  private val orders: Map[String, Json] =
    context.hcursor.downField("orders").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "verify_customer_refund_eligibility"
  override def description: String =
    "Check if the customer order meets the criteria for a refund. Requires customer contact to be verified first."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "order_name" -> Json.obj("type" -> "string".asJson, "description" -> "The name of the customer order.".asJson),
      "contact_email" -> Json.obj("type" -> "string".asJson, "description" -> "The email address of the customer contact.".asJson),
      "verification_status" -> Json.obj("type" -> "string".asJson, "description" -> "Must be \"Verified\" for eligibility check.".asJson),
    ),
    "required" -> List("order_name", "contact_email", "verification_status").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val orderName = arguments.get("order_name").flatMap(_.asString).getOrElse("")
    val contactEmail = arguments.get("contact_email").flatMap(_.asString).getOrElse("")
    val vs = arguments.get("verification_status").flatMap(_.asString).getOrElse("")

    if vs != "Verified" then
      Future.successful(Json.obj("eligibility_status" -> "Ineligible".asJson))
    else
      val order = orders.values.find(o => o.hcursor.downField("name").as[String].contains(orderName))
      order match
        case None => Future.successful(Json.obj("eligibility_status" -> "Ineligible".asJson))
        case Some(o) =>
          val c = o.hcursor
          val emailMatch = c.downField("contact_email").as[String].contains(contactEmail)
          val priceEligible = c.downField("total_price").as[Double].getOrElse(0.0) >= 5
          val status = if emailMatch && priceEligible then "Eligible" else "Ineligible"
          Future.successful(Json.obj("eligibility_status" -> status.asJson))

class CalculateRefundAmount(context: Json) extends Tool:
  private val orders: Map[String, Json] =
    context.hcursor.downField("orders").as[Map[String, Json]].getOrElse(Map.empty)
  private val refundRequests: Map[String, Json] =
    context.hcursor.downField("refund_requests").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "calculate_refund_amount"
  override def description: String = "Calculate the amount of the refund for a customer order."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "order_name" -> Json.obj("type" -> "string".asJson, "description" -> "Name of the customer order.".asJson),
    ),
    "required" -> List("order_name").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val orderName = arguments.get("order_name").flatMap(_.asString).getOrElse("")
    val order = orders.values.find(o => o.hcursor.downField("name").as[String].contains(orderName))
    order match
      case None => Future.successful(Json.obj("refund_request_number" -> "Error: No order found with the provided name".asJson))
      case Some(o) =>
        val c = o.hcursor
        val eligStatus = c.downField("refund_eligibility_status").as[String].getOrElse("")
        if eligStatus != "Eligible" then
          Future.successful(Json.obj("refund_request_number" -> "Error: Order is not eligible for refund".asJson))
        else
          val totalPrice = c.downField("total_price").as[Double].getOrElse(0.0)
          val pct = c.downField("refund_percentage").as[Double].getOrElse(0.0)
          val refundAmount = (totalPrice * pct) / 100
          val rfn = f"RFN-${refundRequests.size + 1}%04d"
          Future.successful(Json.obj(
            "refund_request_number" -> rfn.asJson,
            "refund_amount" -> refundAmount.asJson,
          ))

class InitiateCustomerOrderRefundProcess(context: Json) extends Tool:
  private val orders: Map[String, Json] =
    context.hcursor.downField("orders").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "initiate_customer_order_refund_process"
  override def description: String =
    "Initiate a refund process for a customer order with refund reason and payment method information."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "order_name" -> Json.obj("type" -> "string".asJson, "description" -> "Name of the customer order.".asJson),
      "refund_reason" -> Json.obj("type" -> "string".asJson, "description" -> "Reason for the refund.".asJson),
      "payment_method" -> Json.obj("type" -> "string".asJson, "description" -> "Last four digits of the payment method.".asJson),
    ),
    "required" -> List("order_name", "refund_reason", "payment_method").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val orderName = arguments.get("order_name").flatMap(_.asString).getOrElse("")
    val reason = arguments.get("refund_reason").flatMap(_.asString).getOrElse("")
    val order = orders.values.find(o => o.hcursor.downField("name").as[String].contains(orderName))
    order match
      case None => Future.successful(Json.obj("refund_request_number" -> "Error: No order found with the provided name".asJson))
      case _ =>
        val rfn = f"RFN-${Math.abs(orderName.hashCode) % 10000}%04d"
        Future.successful(Json.obj(
          "refund_request_number" -> rfn.asJson,
          "message" -> s"Refund initiated for order $orderName. Reason: $reason".asJson,
        ))

class ProcessRefund(context: Json) extends Tool:
  private val refundRequests: Map[String, Json] =
    context.hcursor.downField("refund_requests").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "process_refund"
  override def description: String = "Complete the refund process by updating the refund request status to Completed."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "refund_request_number" -> Json.obj("type" -> "string".asJson, "description" -> "Number of the refund request to complete.".asJson),
    ),
    "required" -> List("refund_request_number").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val rfn = arguments.get("refund_request_number").flatMap(_.asString).getOrElse("")
    refundRequests.get(rfn) match
      case None => Future.successful(Json.obj("refund_request_number" -> rfn.asJson, "status" -> "Error: Refund request not found".asJson))
      case _ => Future.successful(Json.obj("refund_request_number" -> rfn.asJson, "status" -> "Completed".asJson))

class GetRefundStatus(context: Json) extends Tool:
  private val refundRequests: Map[String, Json] =
    context.hcursor.downField("refund_requests").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "get_refund_status"
  override def description: String = "Returns refund status for the given refund request number."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "refund_request_number" -> Json.obj("type" -> "string".asJson, "description" -> "Number of the RefundRequest.".asJson),
    ),
    "required" -> List("refund_request_number").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val rfn = arguments.get("refund_request_number").flatMap(_.asString).getOrElse("")
    refundRequests.get(rfn) match
      case None => Future.successful(Json.obj("refund_status" -> "Error: Refund request not found".asJson))
      case Some(r) =>
        val status = r.hcursor.downField("status").as[String].getOrElse("Unknown")
        Future.successful(Json.obj("refund_status" -> status.asJson))

class CancelRefund(context: Json) extends Tool:
  private val refundRequests: Map[String, Json] =
    context.hcursor.downField("refund_requests").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "cancel_refund"
  override def description: String =
    "If the provided refundStatus is not Completed or Cancelled, updates the Refund Request status to Cancelled and returns true; otherwise returns false."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "refund_request_number" -> Json.obj("type" -> "string".asJson, "description" -> "The refund request number to cancel.".asJson),
      "refund_status" -> Json.obj("type" -> "string".asJson, "description" -> "The current refund status value.".asJson),
    ),
    "required" -> List("refund_request_number", "refund_status").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val rfn = arguments.get("refund_request_number").flatMap(_.asString).getOrElse("")
    val status = arguments.get("refund_status").flatMap(_.asString).getOrElse("")
    if status == "Completed" || status == "Cancelled" then
      Future.successful(Json.obj("cancelled" -> false.asJson))
    else refundRequests.get(rfn) match
      case None => Future.successful(Json.obj("cancelled" -> false.asJson))
      case _ => Future.successful(Json.obj("cancelled" -> true.asJson))
