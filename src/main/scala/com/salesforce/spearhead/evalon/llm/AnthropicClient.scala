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

import java.io.FileInputStream
import java.nio.file.Files
import java.security.KeyStore
import java.security.cert.{CertificateFactory, X509Certificate}
import java.util.concurrent.CompletionStage
import javax.net.ssl.{SSLContext, TrustManagerFactory}

import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.FutureConverters.*

import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import sttp.client4.*

import com.salesforce.spearhead.evalon.Settings

class AnthropicClient(
  baseUrl: String = Settings.anthropicBaseUrl,
  apiKey: String = Settings.anthropicApiKey,
  backend: Backend[Future],
)(using ec: ExecutionContext)
    extends Llm:

  private val retryableStatusCodes = Set(429, 502, 503, 529)
  private val maxRetries = 3
  private val baseDelayMs = 1000

  def createMessage(request: CreateMessageRequest): Future[CreateMessageResponse] =
    val requestBody = request.asJson.noSpaces
    sendWithRetry(requestBody)

  override def complete(prompt: String): CompletionStage[String] =
    completeChat("", List(ChatMessage.user(prompt)))

  override def completeChat(system: String, messages: List[ChatMessage]): CompletionStage[String] =
    val request = CreateMessageRequest(
      model = Settings.defaultModel,
      maxTokens = 2048,
      system = system,
      messages = messages.map(_.toJson),
    )
    createMessage(request).map(textOf).asJava

  private def textOf(response: CreateMessageResponse): String =
    response.content.collectFirst { case ContentBlock.TextBlock(t) => t }.getOrElse("")

  private def sendWithRetry(
      requestBody: String,
      attempt: Int = 0
  ): Future[CreateMessageResponse] =
    basicRequest
      .post(uri"$baseUrl/v1/messages")
      .header("x-api-key", apiKey)
      .header("anthropic-version", "2023-06-01")
      .header("content-type", "application/json")
      .body(requestBody)
      .response(asString)
      .send(backend)
      .flatMap { response =>
        response.body match
          case Right(body) =>
            parse(body).flatMap(_.as[CreateMessageResponse]) match
              case Right(msg) => Future.successful(msg)
              case Left(err) =>
                Future.failed(
                  RuntimeException(s"Failed to parse response: $err\nBody: $body")
                )
          case Left(errorBody) if retryableStatusCodes.contains(response.code.code) && attempt < maxRetries =>
            val delay = baseDelayMs * (1 << attempt)
            Future {
              Thread.sleep(delay)
            }.flatMap(_ => sendWithRetry(requestBody, attempt + 1))
          case Left(errorBody) =>
            Future.failed(
              RuntimeException(s"API error (${response.code}): $errorBody")
            )
      }

object AnthropicClient:

  def create()(using ExecutionContext): AnthropicClient =
    val sslContext = Settings.caCertsPath
      .filter(p => Files.exists(java.nio.file.Path.of(p)))
      .map(buildSslContext)

    val httpClient = sslContext match
      case Some(ctx) =>
        java.net.http.HttpClient.newBuilder().sslContext(ctx).build()
      case None =>
        java.net.http.HttpClient.newBuilder().build()

    val backend = sttp.client4.httpclient.HttpClientFutureBackend.usingClient(httpClient)
    new AnthropicClient(backend = backend)

  private def buildSslContext(caCertsPath: String): SSLContext =
    val certFactory = CertificateFactory.getInstance("X.509")
    val fis = new FileInputStream(caCertsPath)
    val certs =
      try certFactory.generateCertificates(fis)
      finally fis.close()

    // Build a keystore with both default and custom certs
    val combinedKs = KeyStore.getInstance(KeyStore.getDefaultType)
    combinedKs.load(null, null)

    // Add custom CA certs
    val iter = certs.iterator()
    var idx = 0
    while iter.hasNext do
      val cert = iter.next().asInstanceOf[X509Certificate]
      combinedKs.setCertificateEntry(s"custom-ca-$idx", cert)
      idx += 1

    // Add system default certs
    val defaultTrustStore = new FileInputStream(
      System.getProperty("java.home") + "/lib/security/cacerts"
    )
    val systemKs = KeyStore.getInstance(KeyStore.getDefaultType)
    try systemKs.load(defaultTrustStore, "changeit".toCharArray)
    finally defaultTrustStore.close()

    val aliases = systemKs.aliases()
    while aliases.hasMoreElements do
      val alias = aliases.nextElement()
      combinedKs.setCertificateEntry(alias, systemKs.getCertificate(alias))

    val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
    tmf.init(combinedKs)

    val sslContext = SSLContext.getInstance("TLS")
    sslContext.init(null, tmf.getTrustManagers, null)
    sslContext
