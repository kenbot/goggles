package goggles.macros.parse

import scalaz.NonEmptyList

private[goggles] object AST {

  case class ComposedLensExpr(exprs: NonEmptyList[LensExpr]) {
    def head: LensExpr = exprs.head
    def tail: List[LensExpr] = exprs.tail.toList
    def toList: List[LensExpr] = exprs.list.toList
  }

  case class AppliedLensExpr(lens: ComposedLensExpr)

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

