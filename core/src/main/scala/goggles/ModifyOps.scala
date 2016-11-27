package goggles

import monocle._


trait ModifyOps[S,T,A,B] {
  def ~=(f: A => B): T

  final def :=(b: B): T =
    this ~= (_ => b)

  final def +=(b: B)(implicit eq: A =:= B, num: Numeric[B]): T =
    this ~= (a => num.plus(eq(a), b))

  final def -=(b: B)(implicit eq: A =:= B, num: Numeric[B]): T =
    this ~= (a => num.minus(eq(a), b))

  final def *=(b: B)(implicit eq: A =:= B, num: Numeric[B]): T =
    this ~= (a => num.times(eq(a), b))

  final def /=(b: B)(implicit eq: A =:= B, num: Fractional[B]): T =
    this ~= (a => num.div(eq(a), b))
}

class MonocleModifyOps[S,T,A,B](targetObj: S, lens: PSetter[S,T,A,B]) extends ModifyOps[S,T,A,B] {
  override def ~=(f: A => B): T = lens.modify(f)(targetObj)
}
