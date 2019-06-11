package goggles.macros.interpret.dsl

import goggles.macros.interpret._
import goggles.macros.interpret.features._
import goggles.macros.interpret.infrastructure._
import goggles.macros.parse._
import goggles.macros.lex.Lexer
import goggles.macros.errors.GogglesError

trait LensModeDslImpl extends LensModeDsl
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


class LensModeDsl  {
  this: Contextual with StringContextInterpreter 
                   with DslModeContext 
                   with LensExprInterpreter => 
                 
  override def mode: DslMode = DslMode.Lens

  def lens(args: c.Expr[Any]*): MacroResult[c.Type, c.Tree] = {

    type Interpret[A] = Parse[c.Type, c.Expr[Any], A]

    val lensExprs: Either[GogglesError[c.Type], AST] =
      Parser.parseUnappliedLens(Lexer(contextStringPartsWithOffsets))

    val finalTree: Interpret[c.Tree] =
      for {
        _ <- Parse.loadLensExprs(lensExprs)
        tree <- interpretAST
      } yield tree

    finalTree.eval(args.toList, mode)
  }

}