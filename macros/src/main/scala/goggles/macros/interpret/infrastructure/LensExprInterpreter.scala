package goggles.macros.interpret.infrastructure

import goggles.macros.interpret._
import goggles.macros.interpret.features._
import goggles.macros.parse._
import goggles.macros.errors.UserError

trait LensExprInterpreter {
    self: Contextual with InterpreterActions 
                     with DslModeContext
                     with NamedLensRefFeature 
                     with InterpolatedLensRefFeature 
                     with EachFeature 
                     with PossibleFeature 
                     with IndexFeature =>
  
    import c.universe._
  
    def interpretAST: Interpret[c.Tree] = {
      val initCode: Interpret[c.Tree] = 
        if (mode.appliedToObject) interpretSourceObject
        else Parse.popLensExpr.flatMap(interpretLensExpr)
  
      initCode.flatMap(compose)
    }

    private def interpretSourceObject: Interpret[c.Tree] = {
      for {
        arg <- Parse.popArg[c.Type, c.Expr[Any]]
        _ <- Parse.storeOpticInfo(OpticInfo(getArgLabel(arg.tree), typeOf[Unit], arg.actualType, OpticType.IsoType, OpticType.IsoType))
      } yield q"_root_.goggles.macros.AppliedObject.const($arg)"
    }

    private def compose(codeSoFar: c.Tree): Interpret[c.Tree] = {
      Parse.popLensExprMaybe[c.Type, c.Expr[Any]].flatMap {
        case None => Parse.pure(codeSoFar)
        case Some(lensExpr) => 
          for {
            lastInfo <- getLastOpticInfo(show(codeSoFar))
            nextLensCode <- interpretLensExpr(lensExpr)
            thisInfo <- getLastOpticInfo(show(nextLensCode))
            tree = q"($codeSoFar).${TermName(thisInfo.opticType.composeVerb)}($nextLensCode)"
            checkedTree <- typeCheckOrElse(tree, UserError.TypesDontMatch(thisInfo.label, thisInfo.sourceType, thisInfo.targetType, lastInfo.targetType, thisInfo.sourceType))
            nextTree <- compose(checkedTree)
          } yield nextTree
      }
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