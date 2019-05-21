package goggles.macros

import goggles.macros.errors._
import goggles.macros.interpret.dsl.{GetModeDslImpl, SetModeDslImpl, LensModeDslImpl}
import goggles.macros.interpret.{MacroResult, OpticType, OpticInfo}
import goggles.macros.lex.Token
import goggles.macros.parse.AST._

import scala.reflect.macros.whitebox


object TestMacros {

  def getImpl(ctx: whitebox.Context)(args: ctx.Expr[Any]*): ctx.Tree = {
    val interpreter = new GetModeDslImpl { override val c: ctx.type = ctx; }
    handleResult(ctx)(interpreter.get(args: _*))
  }

  def setImpl(ctx: whitebox.Context)(args: ctx.Expr[Any]*): ctx.Tree = {
    val interpreter = new SetModeDslImpl { override val c: ctx.type = ctx; }
    handleResult(ctx)(interpreter.set(args: _*))
  }

  def lensImpl(ctx: whitebox.Context)(args: ctx.Expr[Any]*): ctx.Tree = {
    val interpreter = new LensModeDslImpl { override val c: ctx.type = ctx; }
    handleResult(ctx)(interpreter.lens(args: _*))
  }

  private def handleResult(c: whitebox.Context)(
    macroResult: MacroResult[c.Type, c.Tree]): c.Tree = {

    import OpticType._
    import c.universe._

    implicit val lensExprLiftable = Liftable[LensExpr] {
      case RefExpr(NamedLensRef(name)) => q"_root_.goggles.macros.parse.AST.RefExpr(_root_.goggles.macros.parse.AST.NamedLensRef($name))"
      case RefExpr(InterpLensRef) => q"_root_.goggles.macros.parse.AST.RefExpr(_root_.goggles.macros.parse.AST.InterpLensRef)"
      case EachExpr => q"_root_.goggles.macros.parse.AST.EachExpr"
      case OptExpr => q"_root_.goggles.macros.parse.AST.OptExpr"
      case IndexedExpr(LiteralIndex(i)) => q"_root_.goggles.macros.parse.AST.IndexedExpr(_root_.goggles.macros.parse.AST.LiteralIndex($i))"
      case IndexedExpr(InterpIndex) => q"_root_.goggles.macros.parse.AST.IndexedExpr(_root_.goggles.macros.parse.AST.InterpIndex)"
    }

    implicit val opticTypeLiftable = Liftable[OpticType] {
      case FoldType => q"_root_.goggles.macros.interpret.OpticType.FoldType"
      case Fold1Type => q"_root_.goggles.macros.interpret.OpticType.Fold1Type"
      case GetterType => q"_root_.goggles.macros.interpret.OpticType.GetterType"
      case SetterType => q"_root_.goggles.macros.interpret.OpticType.SetterType"
      case TraversalType => q"_root_.goggles.macros.interpret.OpticType.TraversalType"
      case OptionalType => q"_root_.goggles.macros.interpret.OpticType.OptionalType"
      case PrismType => q"_root_.goggles.macros.interpret.OpticType.PrismType"
      case LensType => q"_root_.goggles.macros.interpret.OpticType.LensType"
      case IsoType =>  q"_root_.goggles.macros.interpret.OpticType.IsoType"
    }

    implicit val tokenLiftable = Liftable[Token] {
      case Token.Name(name) => q"_root_.goggles.macros.lex.Token.Name($name)"
      case Token.Dot => q"_root_.goggles.macros.lex.Token.Dot"
      case Token.OpenBracket => q"_root_.goggles.macros.lex.Token.OpenBracket"
      case Token.CloseBracket => q"_root_.goggles.macros.lex.Token.CloseBracket"
      case Token.Star => q"_root_.goggles.macros.lex.Token.Star"
      case Token.Question => q"_root_.goggles.macros.lex.Token.Question"
      case Token.Hole => q"_root_.goggles.macros.lex.Token.Hole"
      case Token.Unrecognised(ch) => q"_root_.goggles.macros.lex.Token.Unrecognised($ch)"
    }

    def typeStr(t: c.Type): String = t.toString

    implicit val errorLiftable = Liftable[GogglesError[c.Type]] {
      case UnrecognisedChar(char) => q"_root_.goggles.macros.errors.UnrecognisedChar($char)"
      case EmptyError => q"_root_.goggles.macros.errors.EmptyError"
      case NameWithNoDot(name) => q"_root_.goggles.macros.errors.NameWithNoDot($name)"
      case InterpOpticWithNoDot => q"_root_.goggles.macros.errors.InterpOpticWithNoDot"
      case InvalidAfterDot(tok) => q"_root_.goggles.macros.errors.InvalidAfterDot($tok)"
      case NonInterpolatedStart(tok) => q"_root_.goggles.macros.errors.NonInterpolatedStart($tok)"
      case UnexpectedCloseBracket => q"_root_.goggles.macros.errors.UnexpectedCloseBracket"
      case EndingDot => q"_root_.goggles.macros.errors.EndingDot"
      case NoIndexSupplied => q"_root_.goggles.macros.errors.NoIndexSupplied"
      case InvalidIndexSupplied(tok) => q"_root_.goggles.macros.errors.InvalidIndexSupplied($tok)"
      case UnclosedOpenBracket => q"_root_.goggles.macros.errors.UnclosedOpenBracket"
      case VerbatimIndexNotInt(expr) => q"_root_.goggles.macros.errors.VerbatimIndexNotPositiveInt($expr)"
      case GetterOpticRequired(finalOpticType) => q"_root_.goggles.macros.errors.GetterOpticRequired($finalOpticType)"
      case SetterOpticRequired(finalOpticType) => q"_root_.goggles.macros.errors.SetterOpticRequired($finalOpticType)"
      case NameNotFound(name, sourceType) => q"_root_.goggles.macros.errors.NameNotFound[String]($name, ${typeStr(sourceType)})"
      case NameNotAMethod(name, sourceType) => q"_root_.goggles.macros.errors.NameNotAMethod[String]($name, ${typeStr(sourceType)})"
      case NameHasArguments(name, sourceType) => q"_root_.goggles.macros.errors.NameHasArguments[String]($name, ${typeStr(sourceType)})"
      case NameHasMultiParamLists(name, onType) => q"_root_.goggles.macros.errors.NameHasMultiParamLists[String]($name, ${typeStr(onType)})"
      case InterpNotAnOptic(name, actualType) => q"_root_.goggles.macros.errors.InterpNotAnOptic[String]($name, ${typeStr(actualType)})"
      case WrongKindOfOptic(name, sourceType, targetType, from, to) => q"_root_.goggles.macros.errors.WrongKindOfOptic($name, ${typeStr(sourceType)}, ${typeStr(targetType)}, $from, $to)"
      case TypesDontMatch(name, sourceType, targetType, expectedType, actualType) => q"_root_.goggles.macros.errors.TypesDontMatch[String]($name, ${typeStr(sourceType)}, ${typeStr(targetType)}, ${typeStr(expectedType)}, ${typeStr(actualType)})"
      case ImplicitEachNotFound(name, sourceType) => q"_root_.goggles.macros.errors.ImplicitEachNotFound[String]($name, ${typeStr(sourceType)})"
      case ImplicitPossibleNotFound(name, sourceType) => q"_root_.goggles.macros.errors.ImplicitPossibleNotFound[String]($name, ${typeStr(sourceType)})"
      case ImplicitIndexNotFound(name, sourceType, indexType) => q"_root_.goggles.macros.errors.ImplicitIndexNotFound[String]($name, ${typeStr(sourceType)}, ${typeStr(indexType)})"
      case CopyMethodNotFound(name, sourceType) => q"_root_.goggles.macros.errors.CopyMethodNotFound[String]($name, ${typeStr(sourceType)})"
      case CopyMethodNotAMethod(name, sourceType) => q"_root_.goggles.macros.errors.CopyMethodNotAMethod[String]($name, ${typeStr(sourceType)})"
      case CopyMethodHasMultiParamLists(name, sourceType) => q"_root_.goggles.macros.errors.CopyMethodHasMultiParamLists[String]($name, ${typeStr(sourceType)})"
      case CopyMethodHasNoArguments(name, sourceType) => q"_root_.goggles.macros.errors.CopyMethodHasNoArguments[String]($name, ${typeStr(sourceType)})"
      case CopyMethodLacksNamedArgument(name, sourceType) => q"_root_.goggles.macros.errors.CopyMethodLacksNamedArgument[String]($name, ${typeStr(sourceType)})"
      case CopyMethodLacksParameterDefaults(name, sourceType, argsWithNoDefault) => q"_root_.goggles.macros.errors.CopyMethodLacksParameterDefaults[String]($name, ${typeStr(sourceType)}, $argsWithNoDefault)"
      case OpticInfoNotFound(label) => q"_root_.goggles.macros.errors.ParseInfoNotFound($label)"
      case UnexpectedEachStructure => q"_root_.goggles.macros.errors.UnexpectedEachStructure"
      case UnexpectedPossibleStructure => q"_root_.goggles.macros.errors.UnexpectedPossibleStructure"
      case UnexpectedIndexStructure(sourceType, indexType) => q"_root_.goggles.macros.errors.UnexpectedIndexStructure[String](${typeStr(sourceType)}, ${typeStr(indexType)})"
      case UnexpectedOpticKind(actualType, numTypeArgs) => q"_root_.goggles.macros.errors.UnexpectedOpticKind[String](${typeStr(actualType)}, $numTypeArgs)"
      case GetVerbNotFound(opticType) => q"_root_.goggles.macros.errors.GetVerbNotFound($opticType)"
      case NotEnoughArguments => q"_root_.goggles.macros.errors.NotEnoughArguments"
    }

    implicit val eitherLiftable = Liftable[Either[GogglesError[c.Type],c.Tree]] {
      case Left(left) => q"_root_.scala.Left($left)"
      case Right(right) => q"_root_.scala.Right($right)"
    }

    implicit val opticInfoLiftable = Liftable[OpticInfo[c.Type]] {
      case OpticInfo(label, sourceType, targetType, opticType, compositeOpticType) => q"_root_.goggles.macros.interpret.OpticInfo[String]($label, ${typeStr(sourceType)}, ${typeStr(targetType)}, $opticType, $compositeOpticType)"
    }

    implicit val macroResultLiftable = Liftable[MacroResult[c.Type, c.Tree]] {
      case MacroResult(errorOrTree, infos, remainingExprs, lastSegmentOffset) => q"_root_.goggles.macros.interpret.MacroResult($errorOrTree, $infos, $remainingExprs, $lastSegmentOffset)"
    }

    q"$macroResult"
  }
}
