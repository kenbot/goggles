package goggles.macros.parse


case class AST(val head: LensExpr, val tail: List[LensExpr]) {
  def exprs: List[LensExpr] = head :: tail
}

sealed trait LensExpr
object LensExpr {
  case class Ref(lens: LensRef) extends LensExpr
  case object Each extends LensExpr
  case object Opt extends LensExpr
  case class Indexed(ix: Index) extends LensExpr
}

sealed trait LensRef
object LensRef {
  case class Named(name: String) extends LensRef
  case object Interpolated extends LensRef
}

sealed trait Index
object Index {
  case class Literal(i: Int) extends Index
  case object Interpolated extends Index
}

