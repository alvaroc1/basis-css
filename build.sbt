name := "basis-css"

organization := "com.gravitydev"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.6"

com.github.retronym.SbtOneJar.oneJarSettings

libraryDependencies ++= Seq(
  "com.gravitydev" %% "extras" % "0.0.4-SNAPSHOT",
  "com.googlecode.kiama" % "kiama_2.11" % "2.0.0-SNAPSHOT",
  "com.lihaoyi" %% "fastparse" % "0.1.5",
  "com.typesafe.play" % "play-json_2.11" % "2.3.9"
)

resolvers ++= Seq(
  "devstack" at "https://devstack.io/repo/gravitydev/public",
  "typesafe" at "http://repo.typesafe.com/typesafe/releases/",
  "nexus" at "https://oss.sonatype.org/content/repositories/snapshots"
)

publishTo := Some("gravitydev" at "https://devstack.io/repo/gravitydev/public")

