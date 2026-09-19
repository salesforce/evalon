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

package com.salesforce.spearhead.evalon.llm

import java.util.concurrent.{CompletableFuture, CompletionStage}

import scala.concurrent.Future
import scala.jdk.FutureConverters.*
import scala.util.control.NonFatal

/** Pluggable language model for simulated participants, the judge, and event sources.
  *
  * Java completion gateways: implement {@link #complete} (SAM) or {@link Llm#blocking}.
  * Chat gateways: override {@link #completeChat} to keep system + turns.
  * The default {@link #completeChat} flattens to a single prompt via {@link Llm#toPrompt}.
  */
@FunctionalInterface
trait Llm:
  def complete(prompt: String): CompletionStage[String]

  def completeChat(system: String, messages: List[ChatMessage]): CompletionStage[String] =
    complete(Llm.toPrompt(system, messages))

object Llm:

  /** Flatten system + turns into one prompt for {@link #complete}. */
  def toPrompt(system: String, messages: List[ChatMessage]): String =
    val turns = messages.map(m => s"${m.role}: ${m.content}").mkString("\n")
    if turns.isEmpty then system else s"$system\n\n$turns"

  /** Java SAM: blocking {@code prompt -> text}. */
  @FunctionalInterface
  trait Blocking:
    def complete(prompt: String): String

  def blocking(fn: Blocking): Llm =
    prompt => CompletableFuture.supplyAsync(() => fn.complete(prompt))

  /** Adapt a raw {@link Llm}'s {@link CompletionStage} to a {@code Future}, capturing a synchronous
    * throw or a null return as a failed {@code Future}. The stage is by-name so the untrusted call
    * runs inside the guard: a raw implementation that throws before returning a stage would
    * otherwise escape the caller's `pipeToSelf`, killing the actor instead of failing the run fast.
    */
  private def toFuture(stage: => CompletionStage[String]): Future[String] =
    try
      val result = stage
      if result == null then
        Future.failed(NullPointerException("Llm returned a null CompletionStage"))
      else result.asScala
    catch case NonFatal(e) => Future.failed(e)

  extension (llm: Llm)
    def completeAsync(prompt: String): Future[String] =
      toFuture(llm.complete(prompt))

    def completeAsync(system: String, messages: List[ChatMessage]): Future[String] =
      toFuture(llm.completeChat(system, messages))
