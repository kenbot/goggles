package goggles.macros

import goggles.macros.errors.Column
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary


object Generators {
  val genColumn: Gen[Column] =
    for {
      heading <- arbitrary[String]
      rawContent <- arbitrary[List[String]]
    } yield Column(heading, rawContent)

  implicit def arbColumn = Arbitrary(genColumn)
}
