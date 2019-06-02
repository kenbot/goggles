package goggles.macros.interpret.dsl

import goggles.macros.interpret._
import goggles.macros.interpret.features._
import goggles.macros.interpret.infrastructure._
import goggles.macros.lex._
import goggles.macros.parse._
import goggles.macros.errors._

trait SetModeDslImpl extends SetModeDsl
  with Contextual 
  with DslModeContext
  with StringContextInterpreter
  with LensExprInterpreter
  with InterpreterActions
  with EachFeature
  with IndexFeature
  with InterpolatedLensRefFeature
  with NamedLensRefFeature
  with PossibleFeature


class SetModeDsl  {
  this: Contextual with StringContextInterpreter 
                   with DslModeContext 
                   with LensExprInterpreter => 

  override def mode: DslMode = DslMode.Set

  def set(args: c.Expr[Any]*): MacroResult[c.Type, c.Tree] = {
    import c.universe._
    import AST._

    def setterExpression(tree: c.Tree): Interpret[c.Tree] = {
      for {
        info <- Parse.getLastOpticInfoOrElse[c.Type, c.Expr[Any]](OpticInfoNotFound(show(tree)))
        tree <- info.compositeOpticType match {
          case OpticType.SetterType => Parse.pure[c.Type, c.Expr[Any], c.Tree](tree)
          case x if x.allowsSet => Parse.pure[c.Type, c.Expr[Any], c.Tree](q"($tree).asSetter")
          case x => Parse.raiseError[c.Type, c.Expr[Any], c.Tree](SetterOpticRequired(x))
        }
      } yield tree
    }

    val lensExprs: Either[GogglesError[c.Type], ComposedLensExpr] =
      Parser.parseAppliedLens(Lexer(contextStringParts))

    val finalTree: Interpret[c.Tree] =
      for {
        _ <- Parse.loadLensExprs(lensExprs)
        tree <- interpretComposedLensExpr
        setter <- setterExpression(tree)
      } yield q"(new _root_.goggles.macros.MonocleModifyOps($setter))"

    finalTree.eval(args.toList, mode)
  }


}