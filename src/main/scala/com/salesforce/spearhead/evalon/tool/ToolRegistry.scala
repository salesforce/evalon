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

import io.circe.Json

object ToolRegistry:
  def build(context: Json): Map[String, Tool] =
    val tools: List[Tool] = List(
      // Flight tools
      FlightLookup(context),
      FlightRebook(context),
      // Hotel tools
      HotelBookingLookup(context),
      HotelCancel(context),
      // Customer support tools
      EscalateCustomerCase(context),
      NotifyCustomerContact(context),
      // Payment issue tools
      VerifyContactIdWithAddr(context),
      ActivatePaymentMethod(context),
      AddPaymentMethod(context),
      BlockFraudTransaction(context),
      CheckNoPendingTransactions(context),
      ConfirmMailingAddress(context),
      DeactivatePaymentMethod(context),
      InitiateRefundTransaction(context),
      RenewPaymentMethod(context),
      RetrieveTransaction(context),
      UpdatePaymentMethod(context),
      ValidatePaymentMethod(context),
      // Order refund tools
      VerifyCustomerContactIdentity(context),
      RetrieveCustomerOrderInformation(context),
      VerifyCustomerRefundEligibility(context),
      CalculateRefundAmount(context),
      InitiateCustomerOrderRefundProcess(context),
      ProcessRefund(context),
      GetRefundStatus(context),
      CancelRefund(context),
    )
    tools.map(t => t.name -> t).toMap
