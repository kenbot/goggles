import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._

resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

enablePlugins(ScalaJSPlugin)

val commonSettings = Seq(
  organization := "com.github.kenbot",
  scalaVersion in ThisBuild := "2.13.0",
  crossScalaVersions := Seq("2.12.8", "2.13.0"),
  scalacOptions += "-Ywarn-unused:imports",
  scalacOptions in (Compile, console) -= "-Ywarn-unused:imports",
  scalacOptions in (Test, console) -= "-Ywarn-unused:imports"
)

lazy val goggles = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .withoutSuffixFor(JVMPlatform)
  .in(file("."))
  .settings(moduleName := "goggles")
  .settings(commonSettings)
  .settings(noPublishSettings)
  .aggregate(dslProject, macrosProject)
  .dependsOn(dslProject, macrosProject)

lazy val macrosProject = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .withoutSuffixFor(JVMPlatform)
  .in(file("macros"))
  .settings(
    commonSettings,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    monocleDeps,
    specs2Deps,
    moduleName := "goggles-macros",
    scalacOptions += "-Yrangepos",
    scalacOptions += "-language:experimental.macros"
  )
  .jvmSettings(
    publishSettings
  )
  .jsSettings(
    noPublishSettings,
    test in Test := streams.value.log.warn(
      "JSPlatform tests disabled in macrosProject since macros are JVM compile-time feature."
    )
  )

val specs2Version = "4.10.3"
val specs2Deps = Seq(
  libraryDependencies += "org.specs2" %%% "specs2-core" % specs2Version % "test",
  libraryDependencies += "org.specs2" %%% "specs2-scalacheck" % specs2Version % "test"
)

val monocleVersion = "1.7.3"
val monocleDeps = Seq(
  libraryDependencies += "com.github.julien-truffaut" %%% "monocle-core" % monocleVersion
)

lazy val dslProject = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .withoutSuffixFor(JVMPlatform)
  .in(file("dsl"))
  .dependsOn(macrosProject)
  .settings(
    commonSettings,
    monocleDeps,
    specs2Deps,
    initialCommandSettings,
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
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
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

lazy val initialCommandSettings = Seq(
  initialCommands := "import goggles._;",
  initialCommands in Test := """
  import goggles._;
  import Fixture._;
  import testdsl._;
"""
)
