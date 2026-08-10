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

package com.salesforce.spearhead.evalon.output

import io.circe.syntax.*

import com.salesforce.spearhead.evalon.model.{EvalResult, TranscriptEntry}

/** Prints transcript entries with ANSI colors and aligned tags. */
class TranscriptPrinter(showConversation: Boolean = false, convWidth: Int = 14):

  private val Reset = "\u001b[0m"
  private val Blue = "\u001b[34m"
  private val Yellow = "\u001b[33m"
  private val Dim = "\u001b[2m"
  private val Red = "\u001b[31m"
  private val defaultColors = List("\u001b[32m", "\u001b[35m", "\u001b[36m", "\u001b[33m")
  private val tagWidth = 17

  private var senderColors: Map[String, String] = Map.empty
  private var lastConversation: Option[String] = None

  private def senderColor(sender: String): String =
    senderColors.getOrElse(
      sender, {
        val color = defaultColors(senderColors.size % defaultColors.size)
        senderColors = senderColors + (sender -> color)
        color
      },
    )

  private def senderIcon(sender: String): String =
    sender.toLowerCase match
      case "agent"          => "🤖 "
      case "end_user"       => "👤 "
      case "representative" => "🎧 "
      case "user"           => "👤 "
      case _                => "💬 "

  private def pad(s: String, width: Int): String = s.padTo(width, ' ')

  private def coloredTag(color: String, label: String): String =
    s"$color${pad(label, tagWidth)}$Reset"

  private def formatLine(tag: String, conversation: Option[String], content: String): String =
    if showConversation then
      val conv = pad(conversation.getOrElse("").take(convWidth), convWidth)
      s"$tag | $conv | $content"
    else s"$tag | $content"

  private def durationSuffix(entry: TranscriptEntry): String =
    entry.durationMs match
      case Some(ms) if ms >= 1000 =>
        val secs = ms / 1000.0
        s"  $Dim(${String.format("%.1f", secs)}s)$Reset"
      case Some(ms) => s"  $Dim(${ms}ms)$Reset"
      case None     => ""

  def apply(entry: TranscriptEntry): Unit =
    // Insert blank line when conversation changes
    if lastConversation.isDefined && entry.conversation != lastConversation then
      println()
    lastConversation = entry.conversation

    val suffix = durationSuffix(entry)

    entry.message.foreach { m =>
      val icon = senderIcon(m.sender)
      val sender = m.sender.toUpperCase
      val color = if m.sender == "agent" then Blue else senderColor(m.sender)
      val tag = coloredTag(color, icon + sender)
      println(formatLine(tag, entry.conversation, m.content) + suffix + "\n")
    }

    entry.toolCall.foreach { tc =>
      val tag = coloredTag(Yellow, "\u2699 CALL")
      val content = s"${tc.toolName}(${tc.arguments.asJson.noSpaces})"
      println(formatLine(tag, entry.conversation, content) + suffix + "\n")
    }

    entry.toolResult.foreach { tr =>
      tr.error match
        case Some(e) =>
          val tag = coloredTag(Red, "\u2699 ERROR")
          println(formatLine(tag, entry.conversation, e) + suffix + "\n")
        case None =>
          val tag = coloredTag(Dim, "\u2699 RESULT")
          val content = s"${tr.result.spaces2}$Reset"
          println(formatLine(tag, entry.conversation, content) + suffix + "\n")
    }

    entry.event.foreach { e =>
      val tag = coloredTag(Dim, "\u26a1 EVENT")
      val content = s"${e.name}: ${e.data.asJson.noSpaces}"
      println(formatLine(tag, entry.conversation, content) + "\n")
    }

object TranscriptPrinter:
  def printEval(result: EvalResult): Unit =
    println("\n=== Evaluation ===\n")
    result.criterionResults.foreach { cr =>
      val status = if cr.passed then "PASS" else "FAIL"
      println(s"  [$status] ${cr.criterion.description}")
      println(f"         Score: ${cr.score}%.1f | ${cr.reasoning}\n")
    }
    println(f"  Overall score: ${result.overallScore}%.2f")
    println(s"  Summary: ${result.summary}\n")
