package goggles.macros

import scala.collection.generic.CanBuildFrom


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

  final def ++=[E](b: B)(implicit eq: A =:= B, toSeq: B <:< Seq[E], cbf: CanBuildFrom[Seq[E], E, B]): T =
    this ~= (a => toSeq(eq(a)) ++ b)
}

