scalaVersion := "2.12.1"

name := "Goggles"
version := "0.1"

scalaVersion in ThisBuild := "2.12.1"
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
    scalacOptions += "-language:experimental.macros"
  )

initialCommands in core := """
  import monocle._, Monocle._;
  import goggles._;

  case class Item(qty: Int);
  case class User(name: String);
  case class ShoppingBasket(user: User, items: List[Item], discount: Option[Int]);

  object DefaultInstances {
    val myItemList = List(Item(11), Item(22), Item(33));
    val myBasket = ShoppingBasket(User("Wally"), myItemList, Some(44));

    val itemQty = Lens[Item, Int](_.qty)(i => _.copy(i));
    val userName = Lens[User, String](_.name)(n => _.copy(n));
    val basketUser = Lens[ShoppingBasket, User](_.user)(u => _.copy(user = u));
    val basketItems = Lens[ShoppingBasket, List[Item]](_.items)(li => _.copy(items = li));
    val basketDiscount = Lens[ShoppingBasket, Option[Int]](_.discount)(oi => _.copy(discount = oi));
    val evenPrism = Prism[Int,Int](i => if (i % 2 == 0) Some(i) else None)(identity);
  }
  import DefaultInstances._;
"""
