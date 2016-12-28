package goggles

import org.specs2._


class GetDslSpec extends Specification with ScalaCheck { def is =
  s2"""
      "get" DSL scenarios:
         get"$$obj.field" $field
         get"$$obj.field1.field2" $fieldField
         get"$$obj.field.$$lens" $fieldLens
         get"$$obj.field*" $fieldStar
         get"$$obj.field?" $fieldQ
         get"$$obj.field?" (failed) $fieldQFailed
         get"$$obj.field[0]" $fieldIndex
         get"$$obj.field[0]" (failed) $fieldIndexFailed
         get"$$obj.$$lens" $lens
         get"$$obj.$$lens.field" $lensField
         get"$$obj.$$lens1.$$lens2" $lensLens
         get"$$obj.$$lens*" $lensStar
         get"$$obj.$$lens?" $lensQ
         get"$$obj.$$lens?" (failed) $lensQFailed
         get"$$obj.$$lens[0]" $lensIndex
         get"$$obj.$$lens[0]" (failed) $lensIndexFailed
         get"$$obj*" $star
         get"$$obj*.field" $starField
         get"$$obj*.$$lens" $starLens
         get"$$obj**" $starStar
         get"$$obj*?" $starQ
         get"$$obj*[0]" $starIndex
         get"$$obj*[0] (failed)" $starIndexFailed
         get"$$obj?" $q
         get"$$obj?" (failed) $qFailed
         get"$$obj?.field" $qField
         get"$$obj?.$$lens" $qLens
         get"$$obj?*" $qStar
         get"$$obj??" $qq
         get"$$obj??" (failed) $qqFailed
         get"$$obj?[0]" $qIndex
         get"$$obj?[0]" (index failed) $qIndexFailed
         get"$$obj[0]" $index
         get"$$obj[0]" (failed) $indexFailed
         get"$$obj[$$i]" $interpIndex
         get"$$obj[0].field" $indexField
         get"$$obj[0].$$lens" $indexLens
         get"$$obj[0]*" $indexStar
         get"$$obj[0]?" $indexQ
         get"$$obj[0]?" (q failed) $indexQFailed
         get"$$obj[0][0]" $indexIndex
   """

  import Fixture._

  def field =
    get"$myBasket.items" === myBasket.items

  def fieldField =
    get"$myBasket.user.name" === "Wally"

  def fieldLens =
    get"$myBasket.user.$userName" === "Wally"

  def fieldStar =
    get"$myBasket.items*" === List(Item(11),Item(22),Item(33))

  def fieldQ =
    get"$myBasket.discount?" === Some(44)

  def fieldQFailed =
    get"${myBasket.copy(discount = None)}.discount?" === None

  def fieldIndex =
    get"$myBasket.items[1]" === Some(Item(22))

  def fieldIndexFailed =
    get"$myBasket.items[66]" === None

  def lens =
    get"$myBasket.$basketItems" === myBasket.items

  def lensField =
    get"$myBasket.$basketUser.name" === "Wally"

  def lensLens =
    get"$myBasket.$basketUser.$userName" === "Wally"

  def lensStar =
    get"$myBasket.$basketItems*" === myItemList

  def lensQ =
    get"$myBasket.$basketDiscount?" === Some(44)

  def lensQFailed =
    get"${myBasket.copy(discount = None)}.$basketDiscount?" === None

  def lensIndex =
    get"$myBasket.$basketItems[1]" === Some(Item(22))

  def lensIndexFailed =
    get"$myBasket.$basketItems[44]" === None

  def star =
    get"${(1,2,3)}*" === List(1,2,3)

  def starField =
    get"$myItemList*.qty" === List(11,22,33)

  def starLens =
    get"$myItemList*.$itemQty" === List(11,22,33)

  def starStar =
    get"${List(List(1,2,3),
               List(4,5,6),
               List(7,8,9))}**" === List(1,2,3,4,5,6,7,8,9)

  def starQ =
    get"${List(Option(1), None, Option(3))}*?" === List(1,3)

  def starIndex =
    get"${List(List(1,2,3),
      List(4,5,6),
      List(7,8,9))}*[2]" === List(7,8,9)

  def starIndexFailed =
    get"${List(List(1,2,3),
      List(4,5,6),
      List(7,8,9))}*[3]" === Nil

  def q =
    get"${Option(44)}?" === Some(44)

  def qFailed =
    get"${Option.empty[Int]}?" === None

  def qField =
    get"${Option(Item(123))}?.qty" === Some(123)

  def qLens =
    get"${Option(Item(123))}?.$itemQty" === Some(123)

  def qStar =
    get"${Option(List(1,2,3))}?*" === List(1,2,3)

  def qq =
    get"${Option(Option(44))}??" === Some(44)

  def qqFailed =
    get"${Option.empty[Option[Int]]}??" === None

  def qIndex =
    get"${Option(List(1,2,3))}?[0]" === Some(1)

  def qIndexFailed =
    get"${Option(List.empty[Int])}?[0]" === None

  def index =
    get"$myItemList[1]" === Some(Item(22))

  def indexFailed =
    get"$myItemList[9]" === None

  def interpIndex = {
    val n = 1
    get"$myItemList[$n]" === Item(22)
  }

  def indexField =
    get"$myItemList[1].qty" === Some(22)

  def indexLens =
    get"$myItemList[1].$itemQty" === Some(22)

  def indexStar =
    get"${List(List(1,2,3),
               List(4,5,6),
               List(7,8,9))}[0]*" === List(1,2,3)

  def indexQ =
    get"${List(Some(1), None, Some(3))}[2]?" === Some(3)

  def indexQFailed =
    get"${List(Some(1), None, Some(3))}[1]?" === None

  def indexIndex =
    get"${List(List(1,2,3),
               List(4,5,6),
               List(7,8,9))}[0][2]" === Some(3)
}
