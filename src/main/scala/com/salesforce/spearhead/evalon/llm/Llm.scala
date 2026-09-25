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

import java.util.concurrent.{CompletableFuture, CompletionStage, Executor, Executors}

import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import scala.jdk.FutureConverters.*
import scala.util.control.NonFatal

/** Pluggable language model for simulated participants, the judge, and event sources.
  *
  * The single method sends a system prompt plus roled turns; how a request is shaped on the wire
  * is entirely the implementation's concern. Callers that only have a bare prompt build the single
  * user message themselves.
  */
@FunctionalInterface
trait Llm:
  def completeChat(system: String, messages: List[ChatMessage]): CompletionStage[String]

object Llm:

  /** SAM for a synchronous client, so Java callers can write `Llm.blocking((system, messages) ->
    * ...)`. `messages` is a {@code java.util.List} because a Scala {@code List} is awkward to iterate
    * from Java; the adapter converts at the boundary. The client owns any flattening it wants.
    */
  @FunctionalInterface
  trait Blocking:
    def completeChat(system: String, messages: java.util.List[ChatMessage]): String

  /** Default executor for [[blocking]] clients: an unbounded cached pool of daemon threads. A
    * synchronous LLM call parks its worker for the whole request, so it must not run on the shared
    * {@code ForkJoinPool.commonPool()} (bounded at ~cores and used by the rest of the JVM) — with
    * many participants plus one judge call per criterion in flight at once, that pool would starve.
    * Daemon threads so this pool never keeps the JVM alive; lazy so nothing is created unless a
    * blocking client is actually used.
    */
  private lazy val blockingExecutor: Executor =
    Executors.newCachedThreadPool { r =>
      val t = Thread(r, "evalon-llm-blocking")
      t.setDaemon(true)
      t
    }

  /** Adapt a blocking chat function into an [[Llm]], running it on [[blockingExecutor]] so it never
    * blocks the calling thread (e.g. a Pekko actor dispatcher). */
  def blocking(fn: Blocking): Llm =
    blocking(fn, blockingExecutor)

  /** As [[blocking]], but run the client on a caller-supplied executor — pass a bounded pool to cap
    * concurrent in-flight calls, or reuse an existing one. */
  def blocking(fn: Blocking, executor: Executor): Llm =
    (system, messages) =>
      CompletableFuture.supplyAsync(() => fn.completeChat(system, messages.asJava), executor)

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
    def completeAsync(system: String, messages: List[ChatMessage]): Future[String] =
      toFuture(llm.completeChat(system, messages))
