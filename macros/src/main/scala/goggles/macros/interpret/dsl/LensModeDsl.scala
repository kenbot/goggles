package goggles.macros.interpret.dsl

import goggles.macros.interpret._
import goggles.macros.interpret.features._
import goggles.macros.interpret.infrastructure._
import goggles.macros.parse._
import goggles.macros.lex.Lexer
import goggles.macros.errors.ErrorAt

trait LensModeDslImpl extends LensModeDsl
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

class LensModeDsl  {
  self: Contextual with DslModeContext
                   with StringContextsContext 
                   with InterpretASTContext =>
                 
  override def mode: DslMode = DslMode.Lens

  def lens(args: c.Expr[Any]*): MacroResult[c.Type, c.Tree] = {

    val astOrError: Either[ErrorAt[c.Type], AST] =
      contextStringFragments(args.toList).flatMap(frags => Parser.parseUnappliedLens(Lexer(frags)))

    val finalTree: Interpret[c.Tree] =
      Parse.fromEither(astOrError).flatMap(interpretAST)

    finalTree.eval(args.toList)
  }
}