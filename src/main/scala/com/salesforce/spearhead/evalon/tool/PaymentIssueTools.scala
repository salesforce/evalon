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

class VerifyContactIdWithAddr(context: Json) extends Tool:
  private val contacts: Map[String, Json] =
    context.hcursor.downField("contacts").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "verify_contact_id_with_addr"

  override def description: String =
    "Verifies the identity of the customer contact by matching the provided email, name, postal code, and state against the contact record."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "email" -> Json.obj("type" -> "string".asJson, "description" -> "Email address of the contact to verify.".asJson),
      "name" -> Json.obj("type" -> "string".asJson, "description" -> "Name of the contact to verify.".asJson),
      "postal_code" -> Json.obj("type" -> "string".asJson, "description" -> "Postal code to verify.".asJson),
      "state" -> Json.obj("type" -> "string".asJson, "description" -> "State to verify.".asJson),
    ),
    "required" -> List("email", "name", "postal_code", "state").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val email = arguments.get("email").flatMap(_.asString).getOrElse("")
    val name = arguments.get("name").flatMap(_.asString).getOrElse("")
    val postalCode = arguments.get("postal_code").flatMap(_.asString).getOrElse("")
    val state = arguments.get("state").flatMap(_.asString).getOrElse("")

    contacts.get(email) match
      case None =>
        Future.successful(Json.obj("verification_status" -> "Error: Contact not found".asJson))
      case Some(contact) =>
        val c = contact.hcursor
        val nameMatch = c.downField("name").as[String].contains(name)
        val pcMatch = c.downField("postal_code").as[String].contains(postalCode)
        val stateMatch = c.downField("state").as[String].contains(state)
        val status = if nameMatch && pcMatch && stateMatch then "Verified" else "NotVerified"
        Future.successful(Json.obj("verification_status" -> status.asJson))

class ActivatePaymentMethod(context: Json) extends Tool:
  private val contacts: Map[String, Json] =
    context.hcursor.downField("contacts").as[Map[String, Json]].getOrElse(Map.empty)
  private val paymentMethods: Map[String, Json] =
    context.hcursor.downField("payment_methods").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "activate_payment_method"
  override def description: String =
    "Activates a payment method for a verified customer. Only one payment method can be active per contact."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "verification_status" -> Json.obj("type" -> "string".asJson, "description" -> "Must be \"Verified\" to proceed.".asJson),
      "last_four_digits" -> Json.obj("type" -> "string".asJson, "description" -> "Last 4 digits of the payment number.".asJson),
      "email" -> Json.obj("type" -> "string".asJson, "description" -> "Email address of the contact.".asJson),
    ),
    "required" -> List("verification_status", "last_four_digits", "email").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val vs = arguments.get("verification_status").flatMap(_.asString).getOrElse("")
    val last4 = arguments.get("last_four_digits").flatMap(_.asString).getOrElse("")
    val email = arguments.get("email").flatMap(_.asString).getOrElse("")

    if vs != "Verified" then
      Future.successful(Json.obj("activation_status" -> s"Error: Customer verification required - status is $vs".asJson))
    else
      contacts.get(email) match
        case None =>
          Future.successful(Json.obj("activation_status" -> "Error: Contact not found".asJson))
        case Some(contact) =>
          val contactId = contact.hcursor.downField("id").as[String].getOrElse("")
          val found = paymentMethods.values.exists { pm =>
            val c = pm.hcursor
            c.downField("contact_id").as[String].contains(contactId) &&
            c.downField("last_four_digits").as[String].contains(last4)
          }
          if !found then
            Future.successful(Json.obj("activation_status" -> "Error: Payment method not found".asJson))
          else
            Future.successful(Json.obj("activation_status" -> s"Successfully activated payment method ending in $last4".asJson))

class AddPaymentMethod(context: Json) extends Tool:
  private val contacts: Map[String, Json] =
    context.hcursor.downField("contacts").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "add_payment_method"
  override def description: String = "Adds a new payment method for a verified customer contact."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "verification_status" -> Json.obj("type" -> "string".asJson, "description" -> "Must be \"Verified\" to proceed.".asJson),
      "email" -> Json.obj("type" -> "string".asJson, "description" -> "Email address of the contact.".asJson),
      "last_four_digits" -> Json.obj("type" -> "string".asJson, "description" -> "Last 4 digits of the payment card.".asJson),
      "billing_address" -> Json.obj("type" -> "string".asJson, "description" -> "Billing address for the payment method.".asJson),
      "expiry_date" -> Json.obj("type" -> "string".asJson, "description" -> "Expiry date (YYYY-MM-DD).".asJson),
      "payment_limit" -> Json.obj("type" -> "number".asJson, "description" -> "Optional spending limit.".asJson),
    ),
    "required" -> List("verification_status", "email", "last_four_digits", "billing_address", "expiry_date").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val vs = arguments.get("verification_status").flatMap(_.asString).getOrElse("")
    val email = arguments.get("email").flatMap(_.asString).getOrElse("")
    val last4 = arguments.get("last_four_digits").flatMap(_.asString).getOrElse("")
    if vs != "Verified" then
      Future.successful(Json.obj("add_status" -> s"Error: Customer verification required - status is $vs".asJson))
    else contacts.get(email) match
      case None => Future.successful(Json.obj("add_status" -> "Error: Contact not found".asJson))
      case _ => Future.successful(Json.obj("add_status" -> s"Successfully added payment method ending in $last4 for contact $email".asJson))

class BlockFraudTransaction(context: Json) extends Tool:
  private val transactions: Map[String, Json] =
    context.hcursor.downField("transactions").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "block_fraud_transaction"
  override def description: String = "Blocks a fraudulent transaction by changing its status from Pending to Declined."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "transaction_number" -> Json.obj("type" -> "string".asJson, "description" -> "The unique identifier of the transaction to block.".asJson),
    ),
    "required" -> List("transaction_number").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val txn = arguments.get("transaction_number").flatMap(_.asString).getOrElse("")
    transactions.get(txn) match
      case None => Future.successful(Json.obj("block_status" -> "Error: Transaction not found".asJson))
      case Some(t) =>
        val status = t.hcursor.downField("status").as[String].getOrElse("")
        if status != "Pending" then
          Future.successful(Json.obj("block_status" -> s"Error: Cannot block transaction - status is $status. Only Pending transactions can be blocked.".asJson))
        else
          Future.successful(Json.obj("block_status" -> s"Successfully blocked transaction $txn".asJson))

class CheckNoPendingTransactions(context: Json) extends Tool:
  private val contacts: Map[String, Json] =
    context.hcursor.downField("contacts").as[Map[String, Json]].getOrElse(Map.empty)
  private val transactions: Map[String, Json] =
    context.hcursor.downField("transactions").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "check_no_pending_transactions"
  override def description: String = "Checks if there are any pending transactions associated with a payment method."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "verification_status" -> Json.obj("type" -> "string".asJson, "description" -> "Must be \"Verified\" to proceed.".asJson),
      "email" -> Json.obj("type" -> "string".asJson, "description" -> "Email address of the contact.".asJson),
      "last_four_digits" -> Json.obj("type" -> "string".asJson, "description" -> "Last 4 digits of the payment number.".asJson),
    ),
    "required" -> List("verification_status", "email", "last_four_digits").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val vs = arguments.get("verification_status").flatMap(_.asString).getOrElse("")
    val email = arguments.get("email").flatMap(_.asString).getOrElse("")
    val last4 = arguments.get("last_four_digits").flatMap(_.asString).getOrElse("")
    if vs != "Verified" then
      Future.successful(Json.obj("check_status" -> s"Error: Customer verification required - status is $vs".asJson))
    else contacts.get(email) match
      case None => Future.successful(Json.obj("check_status" -> "Error: Contact not found".asJson))
      case _ =>
        val hasPending = transactions.values.exists(t => t.hcursor.downField("status").as[String].contains("Pending"))
        val status = if hasPending then s"Warning: Found pending transactions for payment method ending in $last4"
        else s"Confirmed: No pending transactions for payment method ending in $last4"
        Future.successful(Json.obj("check_status" -> status.asJson))

class ConfirmMailingAddress(context: Json) extends Tool:
  private val contacts: Map[String, Json] =
    context.hcursor.downField("contacts").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "confirm_mailing_address"
  override def description: String = "Confirms and updates the mailing address for a contact."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "email" -> Json.obj("type" -> "string".asJson, "description" -> "Email address of the contact.".asJson),
      "mailing_address" -> Json.obj("type" -> "string".asJson, "description" -> "The mailing address to confirm or update.".asJson),
    ),
    "required" -> List("email", "mailing_address").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val email = arguments.get("email").flatMap(_.asString).getOrElse("")
    val addr = arguments.get("mailing_address").flatMap(_.asString).getOrElse("")
    contacts.get(email) match
      case None => Future.successful(Json.obj("confirmation_status" -> "Error: Contact not found".asJson))
      case Some(c) =>
        val stored = c.hcursor.downField("mailing_address").as[String].getOrElse("")
        val status = if stored == addr then s"Mailing address confirmed for $email"
        else s"Mailing address updated to: $addr"
        Future.successful(Json.obj("confirmation_status" -> status.asJson))

class DeactivatePaymentMethod(context: Json) extends Tool:
  private val contacts: Map[String, Json] =
    context.hcursor.downField("contacts").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "deactivate_payment_method"
  override def description: String = "Deactivates a payment method by changing its status to Inactive."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "check_status" -> Json.obj("type" -> "string".asJson, "description" -> "Status from CheckNoPendingTransactions confirming no pending transactions.".asJson),
      "email" -> Json.obj("type" -> "string".asJson, "description" -> "Email address of the contact.".asJson),
      "last_four_digits" -> Json.obj("type" -> "string".asJson, "description" -> "Last 4 digits of the payment number.".asJson),
    ),
    "required" -> List("check_status", "email", "last_four_digits").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val checkStatus = arguments.get("check_status").flatMap(_.asString).getOrElse("")
    val email = arguments.get("email").flatMap(_.asString).getOrElse("")
    val last4 = arguments.get("last_four_digits").flatMap(_.asString).getOrElse("")
    if checkStatus.toLowerCase.contains("pending") then
      Future.successful(Json.obj("deactivation_status" -> "Error: Cannot deactivate - pending transactions exist".asJson))
    else contacts.get(email) match
      case None => Future.successful(Json.obj("deactivation_status" -> "Error: Contact not found".asJson))
      case _ => Future.successful(Json.obj("deactivation_status" -> s"Successfully deactivated payment method ending in $last4".asJson))

class InitiateRefundTransaction(context: Json) extends Tool:
  private val transactions: Map[String, Json] =
    context.hcursor.downField("transactions").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "initiate_refund_transaction"
  override def description: String = "Initiates a refund by creating a refund request for a transaction."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "transaction_number" -> Json.obj("type" -> "string".asJson, "description" -> "The transaction number to initiate refund for.".asJson),
      "refund_amount" -> Json.obj("type" -> "number".asJson, "description" -> "The amount to refund.".asJson),
      "refund_reason" -> Json.obj("type" -> "string".asJson, "description" -> "Reason for the refund.".asJson),
    ),
    "required" -> List("transaction_number", "refund_amount", "refund_reason").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val txn = arguments.get("transaction_number").flatMap(_.asString).getOrElse("")
    val amount = arguments.get("refund_amount").flatMap(_.as[Double].toOption).getOrElse(0.0)
    val reason = arguments.get("refund_reason").flatMap(_.asString).getOrElse("")
    transactions.get(txn) match
      case None => Future.successful(Json.obj("refund_status" -> "Error: Transaction not found".asJson))
      case _ => Future.successful(Json.obj("refund_status" -> s"Refund of $$$amount initiated for transaction $txn. Reason: $reason".asJson))

class RenewPaymentMethod(context: Json) extends Tool:
  private val contacts: Map[String, Json] =
    context.hcursor.downField("contacts").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "renew_payment_method"
  override def description: String = "Renews a payment method by updating its expiry date."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "email" -> Json.obj("type" -> "string".asJson, "description" -> "Email address of the contact.".asJson),
      "last_four_digits" -> Json.obj("type" -> "string".asJson, "description" -> "Last 4 digits of the payment number.".asJson),
      "new_expiry_date" -> Json.obj("type" -> "string".asJson, "description" -> "New expiry date (YYYY-MM-DD).".asJson),
    ),
    "required" -> List("email", "last_four_digits", "new_expiry_date").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val email = arguments.get("email").flatMap(_.asString).getOrElse("")
    val last4 = arguments.get("last_four_digits").flatMap(_.asString).getOrElse("")
    val expiry = arguments.get("new_expiry_date").flatMap(_.asString).getOrElse("")
    contacts.get(email) match
      case None => Future.successful(Json.obj("renewal_status" -> "Error: Contact not found".asJson))
      case _ => Future.successful(Json.obj("renewal_status" -> s"Successfully renewed payment method ending in $last4. New expiry date: $expiry".asJson))

class RetrieveTransaction(context: Json) extends Tool:
  private val contacts: Map[String, Json] =
    context.hcursor.downField("contacts").as[Map[String, Json]].getOrElse(Map.empty)
  private val transactions: Map[String, Json] =
    context.hcursor.downField("transactions").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "retrieve_transaction"
  override def description: String = "Retrieves transaction information based on verification status, email, date, and cost."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "verification_status" -> Json.obj("type" -> "string".asJson, "description" -> "Must be \"Verified\" to proceed.".asJson),
      "email" -> Json.obj("type" -> "string".asJson, "description" -> "Email address of the contact.".asJson),
      "transaction_date" -> Json.obj("type" -> "string".asJson, "description" -> "Date of the transaction (YYYY-MM-DD).".asJson),
      "transaction_amount" -> Json.obj("type" -> "number".asJson, "description" -> "Amount of the transaction.".asJson),
    ),
    "required" -> List("verification_status", "email", "transaction_date", "transaction_amount").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val vs = arguments.get("verification_status").flatMap(_.asString).getOrElse("")
    val email = arguments.get("email").flatMap(_.asString).getOrElse("")
    val amount = arguments.get("transaction_amount").flatMap(_.as[Double].toOption).getOrElse(0.0)
    if vs != "Verified" then
      Future.successful(Json.obj("retrieve_status" -> s"Error: Customer verification required - status is $vs".asJson))
    else contacts.get(email) match
      case None => Future.successful(Json.obj("retrieve_status" -> "Error: Contact not found".asJson))
      case _ =>
        transactions.find { case (_, t) => t.hcursor.downField("amount").as[Double].contains(amount) } match
          case Some((id, t)) =>
            Future.successful(Json.obj(
              "retrieve_status" -> "Transaction found".asJson,
              "transaction_number" -> id.asJson,
              "amount" -> t.hcursor.downField("amount").as[Double].getOrElse(0.0).asJson,
              "status" -> t.hcursor.downField("status").as[String].getOrElse("").asJson,
              "date" -> t.hcursor.downField("date").as[String].getOrElse("").asJson,
            ))
          case None => Future.successful(Json.obj("retrieve_status" -> "Error: Transaction not found".asJson))

class UpdatePaymentMethod(context: Json) extends Tool:
  private val contacts: Map[String, Json] =
    context.hcursor.downField("contacts").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "update_payment_method"
  override def description: String = "Updates a payment method with new billing address and/or payment limit."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "check_status" -> Json.obj("type" -> "string".asJson, "description" -> "Status from CheckNoPendingTransactions.".asJson),
      "email" -> Json.obj("type" -> "string".asJson, "description" -> "Email address of the contact.".asJson),
      "last_four_digits" -> Json.obj("type" -> "string".asJson, "description" -> "Last 4 digits of the payment number.".asJson),
      "new_billing_address" -> Json.obj("type" -> "string".asJson, "description" -> "New billing address.".asJson),
      "new_payment_limit" -> Json.obj("type" -> "number".asJson, "description" -> "New payment limit.".asJson),
    ),
    "required" -> List("check_status", "email", "last_four_digits").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val checkStatus = arguments.get("check_status").flatMap(_.asString).getOrElse("")
    val email = arguments.get("email").flatMap(_.asString).getOrElse("")
    val last4 = arguments.get("last_four_digits").flatMap(_.asString).getOrElse("")
    if checkStatus.toLowerCase.contains("pending") then
      Future.successful(Json.obj("update_status" -> "Error: Cannot update - pending transactions exist".asJson))
    else contacts.get(email) match
      case None => Future.successful(Json.obj("update_status" -> "Error: Contact not found".asJson))
      case _ => Future.successful(Json.obj("update_status" -> s"Successfully updated payment method ending in $last4".asJson))

class ValidatePaymentMethod(context: Json) extends Tool:
  private val contacts: Map[String, Json] =
    context.hcursor.downField("contacts").as[Map[String, Json]].getOrElse(Map.empty)
  private val transactions: Map[String, Json] =
    context.hcursor.downField("transactions").as[Map[String, Json]].getOrElse(Map.empty)

  override def name: String = "validate_payment_method"
  override def description: String = "Validates that a payment method belongs to the correct contact, is active, and has not expired."

  override def parametersSchema: Json = Json.obj(
    "type" -> "object".asJson,
    "properties" -> Json.obj(
      "transaction_number" -> Json.obj("type" -> "string".asJson, "description" -> "The transaction number to validate.".asJson),
      "email" -> Json.obj("type" -> "string".asJson, "description" -> "Email address of the contact.".asJson),
    ),
    "required" -> List("transaction_number", "email").asJson,
  )

  override def execute(arguments: Map[String, Json]): Future[Json] =
    val txn = arguments.get("transaction_number").flatMap(_.asString).getOrElse("")
    val email = arguments.get("email").flatMap(_.asString).getOrElse("")
    contacts.get(email) match
      case None => Future.successful(Json.obj("validation_status" -> "Error: Contact not found".asJson))
      case _ => transactions.get(txn) match
        case None => Future.successful(Json.obj("validation_status" -> "Error: Transaction not found".asJson))
        case _ => Future.successful(Json.obj("validation_status" -> s"Payment method validated for transaction $txn".asJson))
