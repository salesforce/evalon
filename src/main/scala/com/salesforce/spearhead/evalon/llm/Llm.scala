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

import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.FutureConverters.*
import scala.util.control.NonFatal

/**
 * Pluggable language model for simulated participants, the judge, and event sources.
 *
 * The single method sends a system prompt, roled turns and any tools the model may call, and
 * returns the model's text and tool calls. How a request is shaped on the wire is entirely the
 * implementation's concern. Simulated participants always pass tools and act only through the calls
 * they get back, so an implementation must support tool calls; the judge and event sources pass
 * none and read only the text. A reply the model didn't finish (cut off at its token limit) must
 * fail the stage, not come back as a short reply: a participant would read it as silence.
 */
@FunctionalInterface
trait Llm:
  def complete(request: LlmRequest): CompletionStage[LlmResponse]

object Llm:

  /**
   * SAM for a synchronous client, so Java callers can write `Llm.blocking(request -> ...)`. The
   * client owns any flattening it wants.
   */
  @FunctionalInterface
  trait Blocking:
    def complete(request: LlmRequest): LlmResponse

  /**
   * Default executor for [[blocking]] clients: an unbounded cached pool of daemon threads. A
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

  /**
   * Adapt a blocking client into an [[Llm]], running it on [[blockingExecutor]] so it never blocks
   * the calling thread (e.g. a Pekko actor dispatcher).
   */
  def blocking(fn: Blocking): Llm =
    blocking(fn, blockingExecutor)

  /**
   * As [[blocking]], but run the client on a caller-supplied executor — pass a bounded pool to cap
   * concurrent in-flight calls, or reuse an existing one.
   */
  def blocking(fn: Blocking, executor: Executor): Llm =
    request => CompletableFuture.supplyAsync(() => fn.complete(request), executor)

  /**
   * Adapt a raw {@link Llm}'s {@link CompletionStage} to a {@code Future}, capturing a synchronous
   * throw, a null stage, a null response or a null tool call as a failed {@code Future}. The call
   * is by-name so the untrusted code runs inside the guard: a raw implementation that throws before
   * returning a stage would otherwise escape the caller's `pipeToSelf`, killing the actor instead
   * of failing the run fast.
   */
  private def toFuture(stage: => CompletionStage[LlmResponse])(using
      ExecutionContext
  ): Future[LlmResponse] =
    val future =
      try
        val result = stage
        if result == null then
          Future.failed(NullPointerException("Llm returned a null CompletionStage"))
        else result.asScala
      catch case NonFatal(e) => Future.failed(e)
    future.flatMap { response =>
      if response == null then Future.failed(NullPointerException("Llm returned a null response"))
      // A null list means no calls (as in LlmResponse.of), but a null entry is a broken client.
      // Caught here, it fails the call; left in, it would throw inside the caller's actor.
      else if Option(response.toolCalls).exists(_.contains(null)) then
        Future.failed(NullPointerException("Llm returned a null tool call"))
      else Future.successful(response)
    }

  extension (llm: Llm)
    /** Send a request, guarding against a misbehaving implementation (see [[toFuture]]). */
    def completeAsync(request: LlmRequest): Future[LlmResponse] =
      toFuture(llm.complete(request))(using ExecutionContext.parasitic)

    /** A plain text call with no tools, for the judge and event sources. */
    def completeAsync(system: String, messages: List[ChatMessage]): Future[String] =
      completeAsync(LlmRequest(system, messages)).map(_.text)(using ExecutionContext.parasitic)
