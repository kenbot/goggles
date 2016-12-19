package goggles

import monocle.Prism
import org.specs2._

import monocle.std.all._

object Fixture {

  case class Item(qty: Int)
  case class User(name: String)
  case class ShoppingBasket(user: User, items: List[Item], discount: Option[Int])

  val myItemList = List(Item(11), Item(22), Item(33))
  val myBasket = ShoppingBasket(User("Wally"), myItemList, Some(44))
}

class GogglesDslSpec extends Specification with ScalaCheck { def is =
  s2"""
      "get" DSL scenarios:
         get"$$foo.bar" $getDot
         get"$$foo.bar.blah" $getDotDot
         get"$$foo*" $getStar
         get"$$foo**" $getStarStar
         get"$$foo*.bar" $getStarDot
         get"$$foo.bar*" $getDotStar
         get"$$foo?" $getQ
         get"$$foo?" (failed) $getQFailed
         get"$$foo?? $getQQ
         get"$$foo?.bar" $getQDot
         get"$$foo.bar?" $getDotQ
         get"$$foo[0]" $getIndex
         get"$$foo[n]" $getInterpIndex
         get"$$foo[0][0]" $getIndexIndex
         get"$$foo[0].bar" $getIndexDot
         get"$$foo.bar[0]" $getDotIndex
         get"$$foo.bar[0]" (failed) $getDotIndexFailed

     "set" DSL scenarios:
         set"$$foo.bar" ~= f $setDot
         set"$$foo.bar.blah" ~= f $setDotDot
         set"$$foo*" ~= f $setStar
         set"$$foo**" ~= f $setStarStar
         set"$$foo*.bar" ~= f $setStarDot
         set"$$foo.bar*" ~= f $setDotStar
         set"$$foo?" ~= f $setQ
         set"$$foo?" ~= f (failed) $setQFailed
         set"$$foo?? ~= f $setQQ
         set"$$foo?.bar" ~= f $setQDot
         set"$$foo.bar?" ~= f $setDotQ
         set"$$foo[0]" ~= f $setIndex
         set"$$foo[n]" ~= f $setInterpIndex
         set"$$foo[0][0]" ~= f $setIndexIndex
         set"$$foo[0].bar" ~= f $setIndexDot
         set"$$foo.bar[0]" ~= f $setDotIndex
         set"$$foo.bar[0]" ~= f (failed) $setDotIndexFailed
   """

  import Fixture._

  def getDot =
    get"$myBasket.items" === myBasket.items

  def getDotDot =
    get"$myBasket.user.name" === "Wally"

  def getStar =
    get"${(1,2,3)}*" === List(1,2,3)

  def getStarStar =
    get"${List(List(1,2,3),
               List(4,5,6),
               List(7,8,9))}**" === List(1,2,3,4,5,6,7,8,9)

  def getStarDot =
    get"$myItemList*.qty" === List(11,22,33)

  def getDotStar =
    get"$myBasket.items*" === List(Item(11),Item(22),Item(33))

  def getQ =
    get"${Option(44)}?" === Some(44)

  def getQFailed =
    get"${Option.empty}?" === None

  def getQQ =
    get"${Option(Option(44))}??" === Some(44)

  def getQDot =
    get"${Option(Item(123))}?.qty" === Some(123)

  def getDotQ =
    get"$myBasket.discount?" === Some(44)

  def getIndex =
    get"$myItemList[1]" === Item(22)

  def getInterpIndex = {
    val n = 1
    get"$myItemList[$n]" === Item(22)
  }

  def getIndexIndex =
    get"${List(List(1,2,3),
               List(4,5,6),
               List(7,8,9))}[0][2]" === Some(3)

  def getIndexDot =
    get"$myItemList[0].qty" == Some(11)

  def getDotIndex =
    get"$myBasket.items[1]" == Some(Item(22))

  def getDotIndexFailed =
    get"$myBasket.items[66]" == None

  def setDot =
    (set"$myBasket.items" ~= (items => Item(77) :: items)) === myBasket.copy(items = Item(77) :: myItemList)

  def setDotDot =
    (set"$myBasket.user.name" ~= ("Mrs " + _)) === myBasket.copy(user = User("Mrs Wally"))

  def setStar =
    (set"${(1,2,3)}*" ~= (_ + 1)) === List(2,3,4)

  def setStarStar =
    (set"${List(List(1,2,3),
      List(4,5,6),
      List(7,8,9))}**" ~= (_ + 1)) === List(2,3,4,5,6,7,8,9,10)

  def setStarDot =
    (set"$myItemList*.qty" ~= (_ + 1)) === List(12,23,34)

  def setDotStar =
    (set"$myBasket.items*" ~= (it => Item(it.qty * 2))) === List(Item(22),Item(44),Item(66))

  def setQ =
    set"${Option(44)}?" === Some(44)

  def setQFailed =
    get"${Option.empty}?" === None

  def setQQ =
    get"${Option(Option(44))}??" === Some(44)

  def setQDot =
    get"${Option(Item(123))}?.qty" === Some(123)

  def setDotQ =
    get"$myBasket.discount?" === Some(44)

  def setIndex =
    get"$myItemList[1]" === Item(22)

  def setInterpIndex = {
    val n = 1
    get"$myItemList[$n]" === Item(22)
  }

  def setIndexIndex =
    get"${List(List(1,2,3),
      List(4,5,6),
      List(7,8,9))}[0][2]" === Some(3)

  def setIndexDot =
    get"$myItemList[0].qty" == Some(11)

  def setDotIndex =
    get"$myBasket.items[1]" == Some(Item(22))

  def setDotIndexFailed =
    get"$myBasket.items[66]" == None
}
