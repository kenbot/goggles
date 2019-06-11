package goggles.macros.interpret.dsl

import goggles.macros.interpret._
import goggles.macros.interpret.features._
import goggles.macros.interpret.infrastructure._
import goggles.macros.parse._
import goggles.macros.lex.Lexer
import goggles.macros.errors.{GogglesError, UserError, InternalError}

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

  override def mode: DslMode = DslMode.Get

  def get(args: c.Expr[Any]*): MacroResult[c.Type, c.Tree] = {

    val lensExprs: Either[GogglesError[c.Type], AST] =
      Parser.parseAppliedLens(Lexer(contextStringPartsWithOffsets))

    //println(contextStringTrees.map(t => (t.toString, t.pos)).foldLeft("")(_ + _))

    val finalTree =
      for {
        _ <- Parse.loadLensExprs(lensExprs)
        tree <- interpretAST
        info <- Parse.getLastOpticInfoOrElse[c.Type, c.Expr[Any]](InternalError.OpticInfoNotFound(show(tree)))
        verb <- Parse.fromOption(info.compositeOpticType.getVerb, UserError.GetterOpticRequired(info.compositeOpticType))
        postFix = info.compositeOpticType.getVerbPostfix
      } yield mungePostFix(q"($tree).${TermName(verb)}(())", postFix)

    finalTree.eval(args.toList, mode)
  }

  private def mungePostFix(tree: c.Tree, pfix: Option[String]): c.Tree =
    pfix.fold(tree)(str => q"$tree.${TermName(str)}")
}