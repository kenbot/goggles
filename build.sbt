scalaVersion := "2.12.1"

name := "Goggles"
version := "0.1"
organization := "com.github.kenbot"

scalaVersion in ThisBuild := "2.12.1"
crossScalaVersions := Seq("2.11.8", "2.12.1")

resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

lazy val macrosProject = (project in file("macros")).settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  libraryDependencies += "com.github.julien-truffaut" %% "monocle-core" % monocleVersion,
  libraryDependencies ++= specs2Deps,
  moduleName := "goggles-macros",
  scalacOptions += "-Yrangepos",
  scalacOptions += "-language:experimental.macros"
)

val specs2Version = "3.8.6"
val specs2Deps = Seq(
  "org.specs2" %% "specs2-core" % specs2Version % "test",
  "org.specs2" %% "specs2-scalacheck" % specs2Version % "test"
)

val monocleVersion = "1.4.0"
val monocleDeps = Seq(
  "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion
)

lazy val dslProject = (project in file("dsl")).
  dependsOn(macrosProject).
  settings(
    libraryDependencies ++= monocleDeps,
    libraryDependencies ++= specs2Deps,
    moduleName := "goggles-dsl",
    scalacOptions += "-Yrangepos",
    scalacOptions += "-language:experimental.macros"
  )

initialCommands in dslProject := "import goggles._;"

initialCommands in (dslProject, Test) := """
  import goggles._;
  import Fixture._;
"""
