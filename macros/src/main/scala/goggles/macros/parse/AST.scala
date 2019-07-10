package goggles.macros.parse
import goggles.macros.At

case class AST(head: At[LensExpr], tail: List[At[LensExpr]]) {
  def exprs: List[At[LensExpr]] = head :: tail
}

sealed trait LensExpr {
  def isInterpolated: Boolean = false

  def at(offset: Int): At[LensExpr] = At(this, offset)
}

object LensExpr {
  case class Ref(lens: LensRef) extends LensExpr {
    override def isInterpolated: Boolean = lens == LensRef.Interpolated
  }

  case object Each extends LensExpr

  case object Opt extends LensExpr
  
  case class Indexed(index: Index) extends LensExpr {
    override def isInterpolated: Boolean = index == Index.Interpolated
  }
}

sealed trait LensRef
object LensRef {
  case class Named(name: String) extends LensRef
  case object Interpolated extends LensRef
}

sealed trait Index
object Index {
  case class Literal(index: Int) extends Index
  case object Interpolated extends Index
}

