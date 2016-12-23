package goggles

import org.scalacheck._
import org.scalacheck.Arbitrary._
import Fixture._

object Generators {

  case class Index(i: Int)

  val genItem: Gen[Item] =
    arbitrary[Int].map(Item.apply)

  val genUser: Gen[User] =
    arbitrary[String].map(User.apply)

  val genBasket: Gen[ShoppingBasket] =
    for {
      user <- arbitrary[User]
      items <- arbitrary[List[Item]]
      discount <- arbitrary[Option[Int]]
    } yield ShoppingBasket(user, items, discount)

  val genIndex: Gen[Index] =
    Gen.choose(-1, 4).map(Index.apply)

  implicit def arbItem = Arbitrary(genItem)
  implicit def arbUser = Arbitrary(genUser)
  implicit def arbBasket = Arbitrary(genBasket)
  implicit def arbIndex = Arbitrary(genIndex)
}
