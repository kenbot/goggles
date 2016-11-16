scalaVersion := "2.12.0"

name := "Goggles"
version := "0.1"

scalaVersion in ThisBuild := "2.12.0"
run := run in Compile in core

val monocleVersion = "1.3.2"
resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

lazy val macros = (project in file("macros")).settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  libraryDependencies += "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion
)

val monocleDeps = Seq(
    "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
    "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
    "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion,        
    "com.github.julien-truffaut"  %%  "monocle-state"   % monocleVersion,     
    "com.github.julien-truffaut"  %%  "monocle-refined" % monocleVersion,
    "com.github.julien-truffaut"  %%  "monocle-unsafe"  % monocleVersion,
    "com.github.julien-truffaut"  %%  "monocle-law"     % monocleVersion % "test" 
  )

lazy val core = (project in file("core")).dependsOn(macros).settings(
  libraryDependencies ++= monocleDeps,
  addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)
)

