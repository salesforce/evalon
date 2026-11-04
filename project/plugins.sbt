addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.4.1")

addSbtPlugin("com.github.sbt" % "sbt-license-report" % "1.10.0")

addSbtPlugin("com.github.sbt" % "sbt-header" % "5.11.0")

// Tag-driven publishing to Maven Central (bundles sbt-dynver, sbt-pgp, sbt-sonatype)
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.12.1")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.6.2")
