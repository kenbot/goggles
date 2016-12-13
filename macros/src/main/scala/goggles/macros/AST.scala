package goggles.macros

import scalaz.NonEmptyList

object AST {

  case class ComposedLens(exprs: NonEmptyList[LensExpr]) {
    def toList: List[LensExpr] = exprs.list.toList
  }

  case class AppliedLens(lens: ComposedLens)

  sealed trait LensExpr
  case class RefExpr(lens: LensRef) extends LensExpr
  case object EachExpr extends LensExpr
  case object OptExpr extends LensExpr
  case class IndexedExpr(ix: Index) extends LensExpr

  sealed trait LensRef
  case class NamedLensRef(name: String) extends LensRef
  case object InterpLensRef extends LensRef

  sealed trait Index
  case class LiteralIndex(i: Int) extends Index
  case object InterpIndex extends Index
}

