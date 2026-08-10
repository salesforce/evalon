lazy val PekkoVersion = "1.6.0"
lazy val SttpVersion = "4.0.26"

lazy val root = (project in file("."))
  .settings(
    name := "evalon-core",
    organization := "com.salesforce.spearhead",
    scalaVersion := "3.8.2",
    headerLicense := Some(HeaderLicense.Custom(
      """|Copyright (c) 2025, Salesforce, Inc.
         |SPDX-License-Identifier: Apache-2
         |
         |Licensed under the Apache License, Version 2.0 (the "License");
         |you may not use this file except in compliance with the License.
         |You may obtain a copy of the License at
         |
         |    http://www.apache.org/licenses/LICENSE-2.0
         |
         |Unless required by applicable law or agreed to in writing, software
         |distributed under the License is distributed on an "AS IS" BASIS,
         |WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
         |See the License for the specific language governing permissions and
         |limitations under the License.
         |""".stripMargin
    )),
    libraryDependencies ++= Seq(
      // JSON (Circe)
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % "0.14.16") ++
    Seq(
      // YAML
      "io.circe" %% "circe-yaml" % "1.15.0",
    ) ++
    Seq(
      // Logging
      "org.slf4j" % "slf4j-api",
      "org.slf4j" % "slf4j-simple",
    ).map(_ % "2.0.18") ++
    Seq(
      // Pekko (actor framework)
      "org.apache.pekko" %% "pekko-actor-typed" % PekkoVersion,
      "org.apache.pekko" %% "pekko-actor-testkit-typed" % PekkoVersion % Test,

      // HTTP client (sttp)
      "com.softwaremill.sttp.client4" %% "core" % SttpVersion,
      "com.softwaremill.sttp.client4" %% "circe" % SttpVersion,

      // Testing
      "org.scalatest" %% "scalatest" % "3.2.20" % Test,
    ),
    assembly / assemblyMergeStrategy := {
      case PathList(ps @ _*) if ps.last == "module-info.class" => MergeStrategy.discard
      case x =>
        val oldStrategy = (assembly / assemblyMergeStrategy).value
        oldStrategy(x)
    }
  )

 
