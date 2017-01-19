package goggles

import monocle.Lens


object Fixture {

  case class Item(qty: Int)
  case class User(name: String)
  case class ShoppingBasket(user: User, items: List[Item], discount: Option[Int])

  val myItemList = List(Item(11), Item(22), Item(33))
  val myBasket = ShoppingBasket(User("Wally"), myItemList, Some(44))

  val itemQty = Lens[Item, Int](_.qty)(i => _.copy(i))
  val userName = Lens[User, String](_.name)(n => _.copy(n))
  val basketUser = Lens[ShoppingBasket, User](_.user)(u => _.copy(user = u))
  val basketItems = Lens[ShoppingBasket, List[Item]](_.items)(is => _.copy(items = is))
  val basketDiscount = Lens[ShoppingBasket, Option[Int]](_.discount)(oi => _.copy(discount = oi))
}
