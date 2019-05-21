package goggles.macros.interpret.infrastructure

import goggles.macros.interpret._
import goggles.macros.interpret.features._
import goggles.macros.parse._
import goggles.macros.errors._

trait LensExprInterpreter {
    self: Contextual with InterpreterActions 
                     with DslModeContext
                     with NamedLensRefFeature 
                     with InterpolatedLensRefFeature 
                     with EachFeature 
                     with PossibleFeature 
                     with IndexFeature =>
  
    import c.universe._
    import AST._
  
    def interpretComposedLensExpr(composedLens: ComposedLensExpr): Interpret[c.Tree] = {
      val (initCode, lensExprs) =
        if (mode.appliedToObject) (interpretSource, composedLens.exprs)
        else (interpretLensExpr(composedLens.head), composedLens.tail)
  
      initCode.flatMap(composeAll(_, lensExprs))
    }
  
    private def interpretLensExpr(lexpr: LensExpr): Interpret[c.Tree] = {
      lexpr match {
        case RefExpr(NamedLensRef(name)) => interpretNamedLensRef(name)
        case RefExpr(InterpLensRef) => interpretInterpolatedLens
        case EachExpr => interpretEach
        case OptExpr => interpretPossible
        case IndexedExpr(LiteralIndex(i)) => interpretLiteralIndex(i)
        case IndexedExpr(InterpIndex) => interpretInterpolatedIndex
      }
    }
  
    private def interpretSource: Interpret[c.Tree] = {
      for {
        arg <- Parse.popArg[c.Type, c.Expr[Any]]
        _ <- Parse.storeOpticInfo(OpticInfo(getArgLabel(arg.tree), typeOf[Unit], arg.actualType, OpticType.IsoType, OpticType.IsoType))
      } yield q"_root_.goggles.macros.AppliedObject.const($arg)"
    }
  
    private def composeAll(code: c.Tree, lensExprs: List[LensExpr]): Interpret[c.Tree] = {
      lensExprs match {
        case lx :: lxs => compose(code, lx).flatMap(composeAll(_, lxs))
        case Nil => Parse.pure(code)
      }
    }
  
    private def compose(codeSoFar: c.Tree, lensExpr: LensExpr): Interpret[c.Tree] = {
      for {
        lastInfo <- getLastOpticInfo(show(codeSoFar))
        nextLensCode <- interpretLensExpr(lensExpr)
        thisInfo <- getLastOpticInfo(show(nextLensCode))
        tree = q"($codeSoFar).${TermName(thisInfo.opticType.composeVerb)}($nextLensCode)"
        checkedTree <- typeCheckOrElse(tree, TypesDontMatch(thisInfo.label, thisInfo.sourceType, thisInfo.targetType, lastInfo.targetType, thisInfo.sourceType))
      } yield checkedTree
    }
  }