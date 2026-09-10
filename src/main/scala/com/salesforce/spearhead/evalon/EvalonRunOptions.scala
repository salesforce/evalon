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

package com.salesforce.spearhead.evalon

import java.time.Duration
import java.util.concurrent.Executor
import java.util.function.Consumer

import scala.concurrent.ExecutionContext

import com.salesforce.spearhead.evalon.model.{Message, TranscriptEntry}

/** Mutable, Java-friendly options for {@link EvalonRunner}. */
final class EvalonRunOptions:
  private var onEntry: Option[TranscriptEntry => Unit] = None
  private var simulationTimeout: Duration = Duration.ofMinutes(10)
  private var judgeTimeout: Duration = Duration.ofMinutes(5)
  private var executor: Option[Executor] = None

  def withOnEntry(consumer: Consumer[TranscriptEntry]): EvalonRunOptions =
    withOnEntryFn(entry => consumer.accept(entry))

  /** Scala callback; {@link #withOnEntry} is the Java equivalent. */
  def withOnEntryFn(fn: TranscriptEntry => Unit): EvalonRunOptions =
    val prev = onEntry
    onEntry = Some { entry =>
      prev.foreach(_(entry))
      fn(entry)
    }
    this

  def withOnTranscriptLine(consumer: Consumer[String]): EvalonRunOptions =
    withOnEntry { entry =>
      entry.message.foreach { (m: Message) =>
        val conv = entry.conversation.map(c => s"($c) ").getOrElse("")
        consumer.accept(s"$conv[${m.sender}]: ${m.content}")
      }
    }

  def withSimulationTimeout(timeout: Duration): EvalonRunOptions =
    simulationTimeout = timeout
    this

  def getSimulationTimeout: Duration = simulationTimeout

  def withJudgeTimeout(timeout: Duration): EvalonRunOptions =
    judgeTimeout = timeout
    this

  def getJudgeTimeout: Duration = judgeTimeout

  /** Optional thread pool for simulation Futures. Defaults to {@code ExecutionContext.global}. */
  def withExecutor(executor: Executor): EvalonRunOptions =
    this.executor = Option(executor)
    this

  private[evalon] def onEntryFn: Option[TranscriptEntry => Unit] = onEntry

  private[evalon] def executionContext: ExecutionContext =
    executor.map(ExecutionContext.fromExecutor).getOrElse(ExecutionContext.global)
