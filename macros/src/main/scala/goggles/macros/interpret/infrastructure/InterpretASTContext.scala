package goggles.macros.interpret.infrastructure

import goggles.macros.At
import goggles.macros.interpret._
import goggles.macros.interpret.features._
import goggles.macros.parse._
import goggles.macros.errors.UserError

trait InterpretASTContext {
    self: Contextual with DslModeContext
                     with OpticInfoContext
                     with TypeCheckContext
                     with NamedLensRefFeature 
                     with InterpolatedLensRefFeature 
                     with EachFeature 
                     with PossibleFeature 
                     with IndexFeature =>
  
    import c.universe._
  
    def interpretAST(ast: AST): Interpret[c.Tree] = {
      val (initCode, exprs): (Interpret[c.Tree], List[At[LensExpr]]) = 
        if (mode.appliedToObject) (interpretSourceObject, ast.exprs)
        else (interpretLensExpr(ast.head.value), ast.tail)
  
      exprs.foldLeft[Interpret[c.Tree]](initCode) { 
        (interpretTree, atExpr) => interpretTree.flatMap(compose(_, atExpr))
      }
    }

    private def interpretSourceObject: Interpret[c.Tree] = {
      for {
        arg <- Parse.popArg[c.Type, c.Expr[Any]]
        _ <- Parse.storeOpticInfo(OpticInfo(getArgLabel(arg.tree), typeOf[Unit], arg.actualType, OpticType.IsoType, OpticType.IsoType))
      } yield q"_root_.goggles.macros.AppliedObject.const($arg)"
    }

    private def compose(codeSoFar: c.Tree, expr: At[LensExpr]): Interpret[c.Tree] = {
      for {
        _ <- Parse.setCurrentExprOffset(expr.offset)
        lastInfo <- getLastOpticInfo(show(codeSoFar))
        nextLensCode <- interpretLensExpr(expr.value)
        thisInfo <- getLastOpticInfo(show(nextLensCode))
        tree = q"($codeSoFar).${TermName(thisInfo.opticType.composeVerb)}($nextLensCode)"
        checkedTree <- typeCheckOrElse(tree, UserError.TypesDontMatch(thisInfo.label, thisInfo.sourceType, thisInfo.targetType, lastInfo.targetType, thisInfo.sourceType))
      } yield checkedTree
    }

    private def interpretLensExpr(lexpr: LensExpr): Interpret[c.Tree] = {
      lexpr match {
        case LensExpr.Ref(LensRef.Named(name)) => interpretNamedLensRef(name)
        case LensExpr.Ref(LensRef.Interpolated) => interpretInterpolatedLens
        case LensExpr.Each => interpretEach
        case LensExpr.Opt => interpretPossible
        case LensExpr.Indexed(Index.Literal(i)) => interpretLiteralIndex(i)
        case LensExpr.Indexed(Index.Interpolated) => interpretInterpolatedIndex
      }
    }
  }