package goggles

import org.specs2._

import monocle.function.Each._
import monocle.function.Possible._
import monocle.function.Index._

class LensDslSpec extends Specification with ScalaCheck { def is =
  s2"""
     "lens" DSL scenarios:
         lens"$$lens" $justLens
         lens"$$lens.field" $lensField
         lens"$$lens1.$$lens2" $lensLens
         lens"$$lens*" $lensStar
         lens"$$lens?" $lensQ
         lens"$$lens[0]" $lensIntIndex
         lens"$$lens['a']" $lensStringIndex
         lens"$$lens[$$n]" $lensInterpIndex
"""
  /*
     Composition of Monocle optics:
         lens"$$fold.$$fold" $fF
         lens"$$fold.$$getter" $fg
         lens"$$fold.$$setter" $fs
         lens"$$fold.$$traversal" $ft
         lens"$$fold.$$optional" $fo
         lens"$$fold.$$prism" $fp
         lens"$$fold.$$lens" $fl
         lens"$$fold.$$iso" $fi
         lens"$$getter.$$fold" $gf
         lens"$$getter.$$getter" $gg
         lens"$$getter.$$setter" $gs
         lens"$$getter.$$traversal" $gt
         lens"$$getter.$$optional" $go
         lens"$$getter.$$prism" $gp
         lens"$$getter.$$lens" $gl
         lens"$$getter.$$iso" $gi
         lens"$$setter.$$fold" $sf
         lens"$$setter.$$getter" $sg
         lens"$$setter.$$setter" $ss
         lens"$$setter.$$traversal" $st
         lens"$$setter.$$optional" $so
         lens"$$setter.$$prism" $sp
         lens"$$setter.$$lens" $sl
         lens"$$setter.$$iso" $si
         lens"$$traversal.$$fold" $tf
         lens"$$traversal.$$getter" $tg
         lens"$$traversal.$$setter" $ts
         lens"$$traversal.$$traversal" $tt
         lens"$$traversal.$$optional" $to
         lens"$$traversal.$$prism" $to
         lens"$$traversal.$$lens" $tl
         lens"$$traversal.$$iso" $ti
         lens"$$optional.$$fold" $of
         lens"$$optional.$$getter" $og
         lens"$$optional.$$setter" $os
         lens"$$optional.$$traversal" $ot
         lens"$$optional.$$optional" $oo
         lens"$$optional.$$prism" $op
         lens"$$optional.$$lens" $ol
         lens"$$optional.$$iso" $oi
         lens"$$prism.$$fold" $pf
         lens"$$prism.$$getter" $pg
         lens"$$prism.$$setter" $ps
         lens"$$prism.$$traversal" $pt
         lens"$$prism.$$optional" $po
         lens"$$prism.$$prism" $pp
         lens"$$prism.$$lens" $pl
         lens"$$prism.$$iso" $pi
         lens"$$lens.$$fold" $lf
         lens"$$lens.$$getter" $ls
         lens"$$lens.$$setter" $ls
         lens"$$lens.$$traversal" $lt
         lens"$$lens.$$optional" $lo
         lens"$$lens.$$prism" $lp
         lens"$$lens.$$lens" $ll
         lens"$$lens.$$iso" $li
         lens"$$iso.$$fold" $iF
         lens"$$iso.$$getter" $if
         lens"$$iso.$$setter" $iS
         lens"$$iso.$$traversal" $it
         lens"$$iso.$$optional" $io
         lens"$$iso.$$prism" $ip
         lens"$$iso.$$lens" $il
         lens"$$iso.$$iso" $ii
   """

*/

  import Fixture._
  import Generators._

  def justLens = prop { (i: Int) =>
    lens"$itemQty".get(Item(i)) === itemQty.get(Item(i))
  }

  def lensField = prop { (b: ShoppingBasket) =>
    lens"$basketUser.name".get(b) === basketUser.composeLens(userName).get(b)
  }

  def lensLens = prop { (b: ShoppingBasket) =>
    lens"$basketUser.$userName".get(b) === basketUser.composeLens(userName).get(b)
  }

  def lensStar = prop { (b: ShoppingBasket) =>
    lens"$basketItems*".getAll(b) === basketItems.composeTraversal(each).getAll(b)
  }

  def lensQ = prop { (b: ShoppingBasket) =>
    lens"$basketDiscount?".getOption(b) === basketDiscount.composeOptional(possible).getOption(b)
  }

  def lensStringIndex = prop { (b: ShoppingBasket) =>
    lens"$basketUser.flags['banned']".getOption(b) === basketUser.composeOptional(index("banned")).getOption(b)
  }

  def lensIntIndex = prop { (b: ShoppingBasket) =>
    lens"$basketItems[0]".getOption(b) === basketItems.composeOptional(index(0)).getOption(b)
  }

  def lensInterpIndex = prop { (ix: Index, b: ShoppingBasket) =>
    val n = ix.i
    lens"$basketItems[$n]".getOption(b) === basketItems.composeOptional(index(n)).getOption(b)
  }
}
