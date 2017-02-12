package goggles.macros

import goggles.macros.errors.Column
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary


object Generators {
  val genColumn: Gen[Column] =
    for {
      heading <- arbitrary[String]
      rawContent <- arbitrary[Vector[String]]
    } yield Column(heading, rawContent)

  implicit def arbColumn = Arbitrary(genColumn)
}
