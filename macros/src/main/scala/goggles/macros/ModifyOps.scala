package goggles.macros


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

