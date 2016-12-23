package goggles

import monocle.macros.GenLens

object Fixture {

  case class Item(qty: Int)
  case class User(name: String)
  case class ShoppingBasket(user: User, items: List[Item], discount: Option[Int])

  val myItemList = List(Item(11), Item(22), Item(33))
  val myBasket = ShoppingBasket(User("Wally"), myItemList, Some(44))

  val itemQty = GenLens[Item](_.qty)
  val userName = GenLens[User](_.name)
  val basketUser = GenLens[ShoppingBasket](_.user)
  val basketItems = GenLens[ShoppingBasket](_.items)
  val basketDiscount = GenLens[ShoppingBasket](_.discount)
}
