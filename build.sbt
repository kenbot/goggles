scalaVersion := "2.12.0"

name := "Goggles"
version := "0.1"

scalaVersion in ThisBuild := "2.12.0"
run := run in Compile in core

resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

lazy val macros = (project in file("macros")).settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  libraryDependencies += "com.github.julien-truffaut" %% "monocle-core" % monocleVersion,
  libraryDependencies ++= specs2Deps,
  scalacOptions in Test += "-Yrangepos",
  scalacOptions += "-language:experimental.macros"
)

val scalacheckVersion = "3.8.6"
val specs2Deps = Seq(
  "org.specs2" %% "specs2-core" % scalacheckVersion % "test",
  "org.specs2" %% "specs2-scalacheck" % scalacheckVersion % "test"
)

val monocleVersion = "1.3.2"
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
  libraryDependencies ++= specs2Deps,
  scalacOptions += "-language:experimental.macros"
)

initialCommands in core := """
 import monocle._, Monocle._;
 import goggles._;
 import goggles.macros._;

 case class Banana(foo: Int);
 case class Bunch(banana: Banana);
 val bananaFoo = monocle.macros.GenLens[Banana](_.foo);
 val bunchBanana = monocle.macros.GenLens[Bunch](_.banana);
 val myBunch = Bunch(Banana(34));
 val evenPrism = Prism[Int,Int](i => if (i % 2 == 0) Some(i) else None)(i => i)
 val bunches = List(Bunch(Banana(1)), Bunch(Banana(2)), Bunch(Banana(3)))
 
"""
