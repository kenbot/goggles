package goggles.macros

import scala.reflect.macros.whitebox


object AST {

  case class ComposedLens(first: LensExpr, rest: List[LensExpr]) {
    def list = first :: rest
  }

  case class AppliedComposedLens(target: Target, lens: ComposedLens)

  sealed trait LensExpr
  case class RefExpr(lens: LensRef) extends LensExpr
  case object EachExpr extends LensExpr
  case object OptExpr extends LensExpr
  case class IndexedExpr(ix: Index) extends LensExpr

  sealed trait Target
  case class NamedTarget(name: String) extends Target
  case object InterpTarget extends Target

  sealed trait LensRef
  case class NamedLensRef(name: String) extends LensRef
  case object InterpLensRef extends LensRef

  sealed trait Index
  case class LiteralIndex(i: Int) extends Index
  case object InterpIndex extends Index
}

