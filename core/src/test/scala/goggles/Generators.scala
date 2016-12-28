package goggles

import org.scalacheck._
import org.scalacheck.Arbitrary._
import Fixture._

object Generators {


  val genItem: Gen[Item] = arbitrary[Int].map(Item.apply)
  implicit def arbItem = Arbitrary(genItem)

  val genUser: Gen[User] = arbitrary[String].map(User.apply)
  implicit def arbUser = Arbitrary(genUser)

  val genBasket: Gen[ShoppingBasket] =
    for {
      user <- arbitrary[User]
      items <- arbitrary[List[Item]]
      discount <- arbitrary[Option[Int]]
    } yield ShoppingBasket(user, items, discount)

  implicit def arbBasket = Arbitrary(genBasket)

  case class Index(i: Int)
  val genIndex: Gen[Index] = Gen.choose(-1, 4).map(Index.apply)
  implicit def arbIndex = Arbitrary(genIndex)
}
