package goggles.macros.interpret.dsl

import goggles.macros.interpret._
import goggles.macros.interpret.features._
import goggles.macros.interpret.infrastructure._
import goggles.macros.parse._
import goggles.macros.lex.Lexer
import goggles.macros.errors.{UserError, InternalError, ErrorAt}

trait SetModeDslImpl extends SetModeDsl
  with Contextual 
  with DslModeContext
  with StringContextsContext
  with InterpretASTContext
  with TypeCheckContext
  with OpticInfoContext
  with EachFeature
  with IndexFeature
  with InterpolatedLensRefFeature
  with NamedLensRefFeature
  with PossibleFeature
  with HandleResultsContext

class SetModeDsl  {
  self: Contextual with DslModeContext
                   with StringContextsContext 
                   with InterpretASTContext =>

  override def mode: DslMode = DslMode.Set

  def set(args: c.Expr[Any]*): MacroResult[c.Type, c.Tree] = {
    import c.universe._

    def setterExpression(tree: c.Tree): Interpret[c.Tree] = {
      for {
        info <- Parse.getLastOpticInfoOrElse[c.Type, c.Expr[Any]](InternalError.OpticInfoNotFound(show(tree)))
        tree <- info.compositeOpticType match {
          case OpticType.SetterType => Parse.pure[c.Type, c.Expr[Any], c.Tree](tree)
          case x if x.allowsSet => Parse.pure[c.Type, c.Expr[Any], c.Tree](q"($tree).asSetter")
          case x => Parse.raiseError[c.Type, c.Expr[Any], c.Tree](UserError.SetterOpticRequired(x))
        }
      } yield tree
    }

    val astOrError: Either[ErrorAt[c.Type], AST] =
      contextStringFragments(args.toList).flatMap(frags => Parser.parseAppliedLens(Lexer(frags)))

    val finalTree: Interpret[c.Tree] =
      for {
        ast <- Parse.fromEither(astOrError)
        tree <- interpretAST(ast)
        setter <- setterExpression(tree)
      } yield q"(new _root_.goggles.macros.MonocleModifyOps($setter))"

    finalTree.eval(args.toList)
  }
}