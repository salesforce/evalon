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

class EscalateCustomerCase(context: Json) extends Tool:
  override def name: String = "escalate_customer_case"

  override def description: String =
    "Escalate customer case to a senior agent or manager if eligibility is met based on the given policies."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "case_number" -> Json.obj(
        "type" -> "string".asJson,
        "description" -> "The case number to escalate.".asJson,
      ),
      "escalation_reason" -> Json.obj(
        "type" -> "string".asJson,
        "description" -> "Reason for escalation.".asJson,
      ),
    ),
    "required" -> List("case_number", "escalation_reason").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val caseNumber = arguments.get("case_number").flatMap(_.asString).getOrElse("")
    val reason = arguments.get("escalation_reason").flatMap(_.asString).getOrElse("")
    Future.successful(Json.obj(
      "escalation_status" -> s"Case $caseNumber escalated to senior agent. Reason: $reason".asJson
    ))

class NotifyCustomerContact(context: Json) extends Tool:
  override def name: String = "notify_customer_contact"

  override def description: String =
    "Sends an email notification to the specified customer contact."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "contact_email" -> Json.obj(
        "type" -> "string".asJson,
        "description" -> "The customer contact email address to send the notification to.".asJson,
      ),
      "subject" -> Json.obj(
        "type" -> "string".asJson,
        "description" -> "Email subject.".asJson,
      ),
      "body" -> Json.obj(
        "type" -> "string".asJson,
        "description" -> "Email body content.".asJson,
      ),
    ),
    "required" -> List("contact_email", "subject", "body").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val email = arguments.get("contact_email").flatMap(_.asString).getOrElse("")
    val subject = arguments.get("subject").flatMap(_.asString).getOrElse("")
    Future.successful(Json.obj(
      "notification_status" -> s"Success, email sent to $email with subject: $subject".asJson
    ))
