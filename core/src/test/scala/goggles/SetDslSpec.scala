package goggles

import org.specs2._


class SetDslSpec extends Specification with ScalaCheck { def is =
  s2"""
     "set" DSL scenarios:
         set"$$obj.field" ~= f $field
         set"$$obj.field1.field2" ~= f $fieldField
         set"$$obj.field.$$lens" ~= f $fieldLens
         set"$$obj.field*" ~= f $fieldStar
         set"$$obj.field?" ~= f $fieldQ
         set"$$obj.field?" ~= f (failed) $fieldQFailed
         set"$$obj.field[0]" ~= f $fieldIndex
         set"$$obj.field[0]" ~= f (failed) $fieldIndexFailed
         set"$$obj.$$lens" ~= f $lens
         set"$$obj.$$lens.field" ~= f $lensField
         set"$$obj.$$lens1.$$lens2" ~= f $lensLens
         set"$$obj.$$lens*" ~= f $lensStar
         set"$$obj.$$lens?" ~= f $lensQ
         set"$$obj.$$lens?" ~= f (failed) $lensQFailed
         set"$$obj.$$lens[0]" ~= f $lensIndex
         set"$$obj.$$lens[0]" ~= f (failed) $lensIndexFailed
         set"$$obj*" ~= f $star
         set"$$obj*.field" ~= f $starField
         set"$$obj*.$$lens" ~= f $starLens
         set"$$obj**" ~= f $starStar
         set"$$obj*?" ~= f $starQ
         set"$$obj*?" ~= f (failed) $starQFailed
         set"$$obj*[0]" ~= f $starIndex
         set"$$obj*[0] ~= f (failed)" $starIndexFailed
         set"$$obj?" ~= f $q
         set"$$obj?" ~= f (failed) $qFailed
         set"$$obj?.field" ~= f $qField
         set"$$obj?.$$lens" ~= f $qLens
         set"$$obj?*" ~= f $qStar
         set"$$obj??" ~= f $qq
         set"$$obj?[0]" ~= f $qIndex
         set"$$obj?[0]" ~= f (failed) $qIndexFailed
         set"$$obj[0]" ~= f $index
         set"$$obj[0]" ~= f (failed) $indexFailed
         set"$$obj[$$i]" ~= f $interpIndex
         set"$$obj[0].field" ~= f $indexField
         set"$$obj[0].$$lens" ~= f $indexLens
         set"$$obj[0]*" ~= f $indexStar
         set"$$obj[0]?" ~= f $indexQ
         set"$$obj[0]?" ~= f (q failed) $indexQFailed
         set"$$obj[0][0]" ~= f $indexIndex
   """

  import Fixture._
  def field =
    (set"$myBasket.items" ~= (items => Item(77) :: items)) === myBasket.copy(items = Item(77) :: myItemList)

  def fieldField =
    (set"$myBasket.user.name" ~= ("Mrs " + _)) === myBasket.copy(user = User("Mrs Wally"))

  def fieldLens =
    (set"$myBasket.user.$userName" ~= ("Mrs " + _)) === myBasket.copy(user = User("Mrs Wally"))

  def fieldStar =
    (set"$myBasket.items*" ~= (it => Item(it.qty * 2))) === List(Item(22),Item(44),Item(66))

  def fieldQ =
    (set"$myBasket.discount?" += 1) === myBasket.copy(discount = Some(45))

  def fieldQFailed =
    (set"${myBasket.copy(discount=None)}.discount?" += 1) === None

  def fieldIndex =
    (set"$myBasket.items[0]" ~= (i => Item(i.qty + 1))) === myBasket.copy(items = Item(12) :: myItemList.tail)

  def fieldIndexFailed =
    (set"$myBasket.items[11]" ~= (i => Item(i.qty + 1))) === myBasket

  def lens =
    (set"$myBasket.$basketItems" ~= (items => Item(77) :: items)) === myBasket.copy(items = Item(77) :: myItemList)

  def lensField =
    (set"$myBasket.$basketUser.name" ~= ("Mrs " + _)) === myBasket.copy(user = User("Mrs Wally"))

  def lensLens =
    (set"$myBasket.$basketUser.$userName" ~= ("Mrs " + _)) === myBasket.copy(user = User("Mrs Wally"))

  def lensStar =
    (set"$myBasket.$basketItems*" ~= (it => Item(it.qty * 2))) === List(Item(22),Item(44),Item(66))

  def lensQ =
    (set"$myBasket.$basketDiscount?" += 1) === myBasket.copy(discount = Some(45))

  def lensQFailed =
    (set"${myBasket.copy(discount=None)}.$basketDiscount?" += 1) === None

  def lensIndex =
    (set"$myBasket.$basketItems[0]" ~= (i => Item(i.qty + 1))) === myBasket.copy(items = Item(12) :: myItemList.tail)

  def lensIndexFailed =
    (set"$myBasket.$basketItems[11]" ~= (i => Item(i.qty + 1))) === myBasket

  def star =
    (set"${(1,2,3)}*" ~= (_ + 1)) === List(2,3,4)

  def starField =
    (set"$myItemList*.qty" += 1) === List(12,23,34)

  def starLens =
    (set"$myItemList*.$itemQty" += 1) === List(12,23,34)

  def starStar =
    (set"${List(List(1,2,3),
                List(4,5,6),
                List(7,8,9))}**" += 1) === List(List(2,3,4),
                                                List(5,6,7),
                                                List(7,8,9))

  def starQ =
    (set"${List(Some(1), None, Some(3))}*?" += 1) === List(Some(2), None, Some(4))

  def starQFailed =
    (set"$Nil*?" += 1) === Nil

  def starIndex =
    (set"${List(List(1,2,3),
                Nil,
                List(7,8,9))}*[0]" += 1) === List(List(2,2,3),
                                                  Nil,
                                                  List(8,8,9))

  def starIndexFailed =
    (set"${List(List(1,2,3),
                Nil,
                List(7,8,9))}*[4]" += 1) === List(List(1,2,3),
                                                  Nil,
                                                  List(7,8,9))

  def q =
    (set"${Option(44)}?" += 1) === Some(45)

  def qFailed =
    (set"${Option.empty}?" += 1) === None

  def qField =
    (set"${Option(Item(9))}?.qty" += 1) === Some(Item(10))

  def qLens =
    (set"${Option(Item(9))}?.$itemQty" += 1) === Some(Item(10))

  def qStar =
    (set"${Option(List(1,2,3))}?*" += 1) === Some(List(2,3,4))

  def qq =
    (set"${Option(Option(1))}??" += 1) === Some(Some(2))

  def qIndex =
    (set"${Option(List(1,2,3))}?[1]" += 1) === Some(List(1,3,3))

  def qIndexFailed =
    (set"${Option(Nil)}?[1]" += 1) === Some(Nil)

  def index =
    (set"${List(1,2,3)}[2]" += 1) === List(1,2,4)

  def indexFailed =
    (set"${List(1,2,3)}[4]" += 1) === List(1,2,3)

  def interpIndex = {
    val n = 2
    (set"${List(1, 2, 3)}[$n]" += 1) === List(1, 2, 4)
  }

  def indexField =
    (set"$myItemList[0].qty" += 1) === List(Item(12), Item(22), Item(33))

  def indexLens =
    (set"$myItemList[0].$itemQty" += 1) === List(Item(12), Item(22), Item(33))

  def indexStar =
    (set"${List(List(1,2,3),
                List(4,5,6),
                List(7,8,9))}[1]*" += 1) === List(List(1,2,3),
                                                  List(5,6,7),
                                                  List(7,8,9))

  def indexQ =
    (set"${List(Some(1), None, Some(3))}[0]?" += 1) === List(Some(2), None, Some(3))

  def indexQFailed =
    (set"${List(Some(1), None, Some(3))}[1]?" += 1) === List(Some(1), None, Some(3))

  def indexIndex =
    (set"${List(List(1,2,3),
                List(4,5,6),
                List(7,8,9))}[1][2]" += 1) === List(List(1,2,3),
                                                    List(4,5,7),
                                                    List(7,8,9))
}
