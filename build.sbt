scalaVersion := "2.12.1"

name := "Goggles"
version := "0.1"
organization := "com.github.kenbot"

scalaVersion in ThisBuild := "2.12.1"
run := run in Compile in core

resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

lazy val macros = (project in file("macros")).settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  libraryDependencies += "com.github.julien-truffaut" %% "monocle-core" % monocleVersion,
  libraryDependencies ++= specs2Deps,
  scalacOptions += "-Yrangepos",
  scalacOptions += "-language:experimental.macros"
)

val specs2Version = "3.8.6"
val specs2Deps = Seq(
  "org.specs2" %% "specs2-core" % specs2Version % "test",
  "org.specs2" %% "specs2-scalacheck" % specs2Version % "test"
)

val monocleVersion = "1.4.0-M2"
val monocleDeps = Seq(
  "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion
)

lazy val core = (project in file("core")).
  dependsOn(macros).
  settings(
    libraryDependencies ++= monocleDeps,
    libraryDependencies ++= specs2Deps,
    moduleName := "goggles-core",
    scalacOptions += "-Yrangepos",
    scalacOptions += "-language:experimental.macros"
  )

initialCommands in core := "import goggles._;"

initialCommands in (core, Test) := """
  import goggles._;
  import Fixture._;
"""
