package goggles.macros.interpret.dsl

import goggles.macros.interpret._
import goggles.macros.interpret.features._
import goggles.macros.interpret.infrastructure._
import goggles.macros.parse._
import goggles.macros.lex.Lexer
import goggles.macros.errors.{UserError, InternalError, ErrorAt}

trait GetModeDslImpl extends GetModeDsl
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

class GetModeDsl  {
  self: Contextual with DslModeContext
                   with StringContextsContext 
                   with InterpretASTContext =>

  import c.universe._

  override def mode: DslMode = DslMode.Get

  def get(args: c.Expr[Any]*): MacroResult[c.Type, c.Tree] = {

    val astOrError: Either[ErrorAt[c.Type], AST] =
      contextStringFragments(args.toList).flatMap(frags => Parser.parseAppliedLens(Lexer(frags)))

    val finalTree =
      for {
        ast <- Parse.fromEither(astOrError)
        tree <- interpretAST(ast)
        info <- Parse.getLastOpticInfoOrElse[c.Type, c.Expr[Any]](InternalError.OpticInfoNotFound(show(tree)))
        verb <- Parse.fromOption(info.compositeOpticType.getVerb, UserError.GetterOpticRequired(info.compositeOpticType))
        postFix = info.compositeOpticType.getVerbPostfix
      } yield mungePostFix(q"($tree).${TermName(verb)}(())", postFix)

    finalTree.eval(args.toList)
  }

  private def mungePostFix(tree: c.Tree, pfix: Option[String]): c.Tree =
    pfix.fold(tree)(str => q"$tree.${TermName(str)}")
}