package goggles.macros.interpret.dsl

import goggles.macros.interpret._
import goggles.macros.interpret.features._
import goggles.macros.interpret.infrastructure._
import goggles.macros.lex._
import goggles.macros.parse._
import goggles.macros.errors._

trait GetModeDslImpl extends GetModeDsl
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



class GetModeDsl  {
  self: Contextual with DslModeContext
                   with StringContextInterpreter 
                   with LensExprInterpreter =>

  import c.universe._
  import AST._

  override def mode: DslMode = DslMode.Get

  def get(args: c.Expr[Any]*): MacroResult[c.Type, c.Tree] = {
    val errorOrAst: Either[GogglesError[c.Type], ComposedLensExpr] =
      Parser.parseAppliedLens(Lexer(contextStringParts))

    val finalTree =
      for {
        ast <- Parse.fromEither(errorOrAst)
        tree <- interpretComposedLensExpr(ast)
        info <- Parse.getLastOpticInfoOrElse[c.Type, c.Expr[Any]](OpticInfoNotFound(show(tree)))
        verb <- Parse.fromOption(info.compositeOpticType.getVerb, GetterOpticRequired(info.compositeOpticType))
        postFix = info.compositeOpticType.getVerbPostfix
      } yield mungePostFix(q"($tree).${TermName(verb)}(())", postFix)

    val (errorOrTree, macroState) = finalTree.eval(args.toList)
    MacroResult(errorOrTree, macroState.infos, Nil, SourcePosition.getErrorOffset(mode, macroState))
  }

  private def mungePostFix(tree: c.Tree, pfix: Option[String]): c.Tree =
    pfix.fold(tree)(str => q"$tree.${TermName(str)}")
}