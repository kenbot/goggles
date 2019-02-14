package goggles.macros.interpret

import goggles.macros.interpret.features._
import goggles.macros.lex._
import goggles.macros.parse._
import goggles.macros.errors._

trait GetModeImpl extends GetModeInterpreter 
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



class GetModeInterpreter  {
  self: Contextual with DslModeContext
                   with StringContextInterpreter 
                   with LensExprInterpreter =>

  import c.universe._
  import AST._

  override def mode: DslMode = DslMode.Get

  def get(args: c.Expr[Any]*): MacroResult[c.Type, c.Tree] = {
    val errorOrAst: Either[GogglesError[c.Type], AppliedLensExpr] =
      Parser.parseAppliedLens(Lexer(contextStringParts))

    val finalTree =
      for {
        ast <- Parse.fromEither(errorOrAst)
        tree <- interpretComposedLensExpr(ast.lens)
        info <- Parse.getLastOpticInfoOrElse[c.Type, c.Expr[Any]](OpticInfoNotFound(show(tree)))
        verb <- Parse.fromOption(info.compositeOpticType.getVerb, GetterOpticRequired(info.compositeOpticType))
        postFix = info.compositeOpticType.getVerbPostfix
      } yield mungePostFix(q"($tree).${TermName(verb)}(())", postFix)

    val (errorOrTree, infos) = finalTree.eval(args.toList)
    MacroResult(errorOrTree, infos, SourcePosition.getErrorOffset(mode, infos))
  }

  private def mungePostFix(tree: c.Tree, pfix: Option[String]): c.Tree =
    pfix.fold(tree)(str => q"$tree.${TermName(str)}")
}