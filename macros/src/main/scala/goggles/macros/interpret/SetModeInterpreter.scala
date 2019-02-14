package goggles.macros.interpret

import goggles.macros.interpret.features._
import goggles.macros.lex._
import goggles.macros.parse._
import goggles.macros.errors._

trait SetModeImpl extends SetModeInterpreter 
  with Contextual 
  with DslModeContext
  with StringContextInterpreter
  with LensExprInterpreter
  with InterpreterTools
  with EachFeature
  with IndexFeature
  with InterpolatedLensRefFeature
  with NamedLensRefFeature
  with PossibleFeature


class SetModeInterpreter  {
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

    val errorOrAst: Either[GogglesError[c.Type], AppliedLensExpr] =
      Parser.parseAppliedLens(Lexer(contextStringParts))

    val finalTree: Interpret[c.Tree] =
      for {
        ast <- Parse.fromEither(errorOrAst)
        tree <- interpretComposedLensExpr(ast.lens)
        setter <- setterExpression(tree)
      } yield q"(new _root_.goggles.macros.MonocleModifyOps($setter))"

    val (errorOrTree, infos) = finalTree.eval(args.toList)
    MacroResult(errorOrTree, infos, SourcePosition.getErrorOffset(mode, infos))
  }


}