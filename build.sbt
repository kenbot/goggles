import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._

resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

val commonSettings = Seq(
  organization := "com.github.kenbot",
  scalaVersion in ThisBuild := "2.12.7",
  crossScalaVersions := Seq("2.11.8", "2.12.7"),
  scalacOptions += "-Ywarn-unused-import",
  scalacOptions in (Compile, console) -= "-Ywarn-unused-import",
  scalacOptions in (Test, console) -= "-Ywarn-unused-import"
)

lazy val goggles = project.in(file("."))
  .settings(moduleName := "goggles")
  .settings(commonSettings)
  .settings(noPublishSettings)
  .aggregate(dslProject, macrosProject)
  .dependsOn(dslProject, macrosProject)

lazy val macrosProject = (project in file("macros")).settings(
  commonSettings,
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  libraryDependencies += "com.github.julien-truffaut" %% "monocle-core" % monocleVersion,
  libraryDependencies ++= specs2Deps,
  publishSettings,
  moduleName := "goggles-macros",
  scalacOptions += "-Yrangepos",
  scalacOptions += "-language:experimental.macros"
)

val specs2Version = "3.8.6"
val specs2Deps = Seq(
  "org.specs2" %% "specs2-core" % specs2Version % "test",
  "org.specs2" %% "specs2-scalacheck" % specs2Version % "test"
)

val monocleVersion = "1.5.0"
val monocleDeps = Seq(
  "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion
)

lazy val dslProject = (project in file("dsl")).
  dependsOn(macrosProject).
  settings(
    commonSettings,
    libraryDependencies ++= monocleDeps,
    libraryDependencies ++= specs2Deps,
    publishSettings,
    moduleName := "goggles-dsl",
    scalacOptions += "-Yrangepos",
    scalacOptions += "-language:experimental.macros"
  )

lazy val noPublishSettings = Seq(
  publish := {},
  publishTo := Some(Resolver.file("delete-me", file("delete-me"))),
  publishLocal := {},
  publishArtifact := false,
  skip in publish := true
)

val publishSettings = Seq(
  publishMavenStyle := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  sonatypeProfileName := "com.github.kenbot",
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  pomExtra := {
    <url>https://github.com/kenbot/goggles</url>
    <scm>
      <url>git@github.com:kenbot/goggles</url>
      <connection>scm:git:git@github.com:kenbot/goggles.git</connection>
    </scm>
    <licenses>
      <license>
        <name>MIT</name>
        <url>https://opensource.org/licenses/MIT</url>
      </license>
    </licenses>
    <developers>
      <developer>
        <id>kenbot</id>
        <name>Ken Scambler</name>
        <url>https://github.com/kenbot</url>
      </developer>
    </developers>
  },
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    releaseStepCommandAndRemaining("publish"),
    setNextVersion,
    commitNextVersion,
    pushChanges
  )
)

initialCommands in dslProject := "import goggles._;"

initialCommands in (dslProject, Test) := """
  import goggles._;
  import Fixture._;
  import testdsl._;
"""
