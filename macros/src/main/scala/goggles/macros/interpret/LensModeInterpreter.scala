package goggles.macros.interpret

import goggles.macros.interpret.features._
import goggles.macros.lex._
import goggles.macros.parse._
import goggles.macros.errors._

trait LensModeImpl extends LensModeInterpreter 
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


class LensModeInterpreter  {
  this: Contextual with StringContextInterpreter 
                   with DslModeContext 
                   with LensExprInterpreter => 

  import AST._
                 
  override def mode: DslMode = DslMode.Lens

  def lens(args: c.Expr[Any]*): MacroResult[c.Type, c.Tree] = {

    type Interpret[A] = Parse[c.Type, c.Expr[Any], A]

    val errorOrAst: Either[GogglesError[c.Type], ComposedLensExpr] =
      Parser.parseUnappliedLens(Lexer(contextStringParts))

    val finalTree: Interpret[c.Tree] =
      for {
        ast <- Parse.fromEither(errorOrAst)
        tree <- interpretComposedLensExpr(ast)
      } yield tree

      val (errorOrTree, infos) = finalTree.eval(args.toList)
      MacroResult(errorOrTree, infos, SourcePosition.getErrorOffset(DslMode.Lens, infos))
  }

}