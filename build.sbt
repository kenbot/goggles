import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._

resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

val commonSettings = Seq(
  organization := "com.github.kenbot",
  scalaVersion in ThisBuild := "2.12.4",
  crossScalaVersions := Seq("2.11.8", "2.12.4"),
  scalacOptions += "-Ywarn-unused-import",
  scalacOptions in (Compile, console) -= "-Ywarn-unused-import",
  scalacOptions in (Test, console) -= "-Ywarn-unused-import"
)

lazy val goggles = project.in(file("."))
  .settings(moduleName := "goggles")
  .settings(commonSettings)
  .settings(noPublishSettings)
  .aggregate(gogglesJVM, gogglesJS)
  .dependsOn(gogglesJVM, gogglesJS)

lazy val gogglesJVM = project.in(file(".gogglesJVM"))
  .settings(commonSettings)
  .settings(noPublishSettings)
  .aggregate(dslProjectJVM, macrosProjectJVM)
  .dependsOn(dslProjectJVM, macrosProjectJVM)

lazy val gogglesJS = project.in(file(".gogglesJS"))
  .settings(commonSettings)
  .settings(noPublishSettings)
  .aggregate(dslProjectJS, macrosProjectJS)
  .dependsOn(dslProjectJS, macrosProjectJS)

lazy val macrosProjectJVM = macrosProject.jvm
lazy val macrosProjectJS = macrosProject.js
lazy val macrosProject = (crossProject in file("macros")).
  settings(
    commonSettings,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    libraryDependencies += monocleCore.value,
    publishSettings,
    moduleName := "goggles-macros",
    scalacOptions += "-Yrangepos",
    scalacOptions += "-language:experimental.macros"
  ).
  jvmSettings(
    libraryDependencies ++= specs2Deps
  )

val specs2Version = "3.8.6"
val specs2Deps = Seq(
  "org.specs2" %% "specs2-core" % specs2Version % "test",
  "org.specs2" %% "specs2-scalacheck" % specs2Version % "test"
)

val monocleVersion = "1.5.0"
val monocleCore = Def.setting("com.github.julien-truffaut" %%% "monocle-core" % monocleVersion)


lazy val dslProjectJVM = dslProject.jvm
lazy val dslProjectJS = dslProject.js
lazy val dslProject = (crossProject in file("dsl")).
  dependsOn(macrosProject).
  settings(
    commonSettings,
    libraryDependencies += monocleCore.value,
    publishSettings,
    moduleName := "goggles-dsl",
    scalacOptions += "-Yrangepos",
    scalacOptions += "-language:experimental.macros",
    initialCommands := "import goggles._;",
    initialCommands in Test := """
      import goggles._;
      import Fixture._;
    """
  ).
  jvmSettings(
    libraryDependencies ++= specs2Deps
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
