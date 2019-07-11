package goggles.macros

import goggles.macros.errors.{GogglesError, SyntaxError, UserError, InternalError, ErrorAt}
import goggles.macros.interpret.dsl.{GetModeDslImpl, SetModeDslImpl, LensModeDslImpl}
import goggles.macros.interpret.{MacroResult, OpticType, OpticInfo}
import goggles.macros.lex.Token
import goggles.macros.parse.{LensExpr, LensRef, Index}

import scala.reflect.macros.whitebox


object RuntimeMacros {

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
      case LensExpr.Ref(LensRef.Named(name)) => q"_root_.goggles.macros.parse.LensExpr.Ref(_root_.goggles.macros.parse.LensRef.Named($name))"
      case LensExpr.Ref(LensRef.Interpolated) => q"_root_.goggles.macros.parse.LensExpr.Ref(_root_.goggles.macros.parse.LensRef.Interpolated)"
      case LensExpr.Each => q"_root_.goggles.macros.parse.LensExpr.Each"
      case LensExpr.Opt => q"_root_.goggles.macros.parse.LensExpr.Opt"
      case LensExpr.Indexed(Index.Literal(i)) => q"_root_.goggles.macros.parse.LensExpr.Indexed(_root_.goggles.macros.parse.Index.Literal($i))"
      case LensExpr.Indexed(Index.Interpolated) => q"_root_.goggles.macros.parse.LensExpr.Indexed(_root_.goggles.macros.parse.Index.Interpolated)"
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
      case SyntaxError.UnrecognisedChar(char) => q"_root_.goggles.macros.errors.SyntaxError.UnrecognisedChar($char)"
      case SyntaxError.EmptyError => q"_root_.goggles.macros.errors.SyntaxError.EmptyError"
      case SyntaxError.NameWithNoDot(name) => q"_root_.goggles.macros.errors.SyntaxError.NameWithNoDot($name)"
      case SyntaxError.InterpOpticWithNoDot => q"_root_.goggles.macros.errors.SyntaxError.InterpOpticWithNoDot"
      case SyntaxError.InvalidAfterDot(token) => q"_root_.goggles.macros.errors.SyntaxError.InvalidAfterDot($token)"
      case SyntaxError.NonInterpolatedStart(token) => q"_root_.goggles.macros.errors.SyntaxError.NonInterpolatedStart($token)"
      case SyntaxError.UnexpectedCloseBracket => q"_root_.goggles.macros.errors.SyntaxError.UnexpectedCloseBracket"
      case SyntaxError.EndingDot => q"_root_.goggles.macros.errors.SyntaxError.EndingDot"
      case SyntaxError.NoIndexSupplied  => q"_root_.goggles.macros.errors.SyntaxError.NoIndexSupplied"
      case SyntaxError.InvalidIndexSupplied(token) => q"_root_.goggles.macros.errors.SyntaxError.InvalidIndexSupplied($token)"
      case SyntaxError.UnclosedOpenBracket => q"_root_.goggles.macros.errors.SyntaxError.UnclosedOpenBracket"
      case SyntaxError.VerbatimIndexNotInt(expr) => q"_root_.goggles.macros.errors.SyntaxError.VerbatimIndexNotPositiveInt($expr)"
      case UserError.GetterOpticRequired(finalOpticType) => q"_root_.goggles.macros.errors.UserError.GetterOpticRequired($finalOpticType)"
      case UserError.SetterOpticRequired(finalOpticType) => q"_root_.goggles.macros.errors.UserError.SetterOpticRequired($finalOpticType)"
      case UserError.NameNotFound(name, sourceType) => q"_root_.goggles.macros.errors.UserError.NameNotFound[String]($name, ${typeStr(sourceType)})"
      case UserError.NameNotAMethod(name, sourceType) => q"_root_.goggles.macros.errors.UserError.NameNotAMethod[String]($name, ${typeStr(sourceType)})"
      case UserError.NameHasArguments(name, sourceType) => q"_root_.goggles.macros.errors.UserError.NameHasArguments[String]($name, ${typeStr(sourceType)})"
      case UserError.NameHasMultiParamLists(name, onType) => q"_root_.goggles.macros.errors.UserError.NameHasMultiParamLists[String]($name, ${typeStr(onType)})"
      case UserError.InterpNotAnOptic(name, actualType) => q"_root_.goggles.macros.errors.UserError.InterpNotAnOptic[String]($name, ${typeStr(actualType)})"
      case UserError.WrongKindOfOptic(name, sourceType, targetType, from, to) => q"_root_.goggles.macros.errors.UserError.WrongKindOfOptic($name, ${typeStr(sourceType)}, ${typeStr(targetType)}, $from, $to)"
      case UserError.TypesDontMatch(name, sourceType, targetType, expectedType, actualType) => q"_root_.goggles.macros.errors.UserError.TypesDontMatch[String]($name, ${typeStr(sourceType)}, ${typeStr(targetType)}, ${typeStr(expectedType)}, ${typeStr(actualType)})"
      case UserError.ImplicitEachNotFound(name, sourceType) => q"_root_.goggles.macros.errors.UserError.ImplicitEachNotFound[String]($name, ${typeStr(sourceType)})"
      case UserError.ImplicitPossibleNotFound(name, sourceType) => q"_root_.goggles.macros.errors.UserError.ImplicitPossibleNotFound[String]($name, ${typeStr(sourceType)})"
      case UserError.ImplicitIndexNotFound(name, sourceType, indexType) => q"_root_.goggles.macros.errors.UserError.ImplicitIndexNotFound[String]($name, ${typeStr(sourceType)}, ${typeStr(indexType)})"
      case UserError.CopyMethodNotFound(name, sourceType) => q"_root_.goggles.macros.errors.UserError.CopyMethodNotFound[String]($name, ${typeStr(sourceType)})"
      case UserError.CopyMethodNotAMethod(name, sourceType) => q"_root_.goggles.macros.errors.UserError.CopyMethodNotAMethod[String]($name, ${typeStr(sourceType)})"
      case UserError.CopyMethodHasMultiParamLists(name, sourceType) => q"_root_.goggles.macros.errors.UserError.CopyMethodHasMultiParamLists[String]($name, ${typeStr(sourceType)})"
      case UserError.CopyMethodHasNoArguments(name, sourceType) => q"_root_.goggles.macros.errors.UserError.CopyMethodHasNoArguments[String]($name, ${typeStr(sourceType)})"
      case UserError.CopyMethodLacksNamedArgument(name, sourceType) => q"_root_.goggles.macros.errors.UserError.CopyMethodLacksNamedArgument[String]($name, ${typeStr(sourceType)})"
      case UserError.CopyMethodLacksParameterDefaults(name, sourceType, argsWithNoDefault) => q"_root_.goggles.macros.errors.UserError.CopyMethodLacksParameterDefaults[String]($name, ${typeStr(sourceType)}, $argsWithNoDefault)"
      case InternalError.OpticInfoNotFound(label) => q"_root_.goggles.macros.errors.InternalError.OpticInfoNotFound($label)"
      case InternalError.UnexpectedEachStructure => q"_root_.goggles.macros.errors.InternalError.UnexpectedEachStructure"
      case InternalError.UnexpectedPossibleStructure => q"_root_.goggles.macros.errors.InternalError.UnexpectedPossibleStructure"
      case InternalError.UnexpectedIndexStructure(sourceType, indexType) => q"_root_.goggles.macros.errors.InternalError.UnexpectedIndexStructure[String](${typeStr(sourceType)}, ${typeStr(indexType)})"
      case InternalError.UnexpectedOpticKind(actualType, numTypeArgs) => q"_root_.goggles.macros.errors.InternalError.UnexpectedOpticKind[String](${typeStr(actualType)}, $numTypeArgs)"
      case InternalError.GetVerbNotFound(opticType) => q"_root_.goggles.macros.errors.InternalError.GetVerbNotFound($opticType)"
      case InternalError.NotEnoughArguments => q"_root_.goggles.macros.errors.InternalError.NotEnoughArguments"
    }

    implicit val errorAtLiftable = Liftable[ErrorAt[c.Type]] {
      case ErrorAt(error, offset) => q"_root_.goggles.macros.errors.ErrorAt($error, $offset)"
    }

    implicit val eitherLiftable = Liftable[Either[ErrorAt[c.Type],c.Tree]] {
      case Left(left) => q"_root_.scala.Left($left)"
      case Right(right) => q"_root_.scala.Right($right)"
    }

    implicit val opticInfoLiftable = Liftable[OpticInfo[c.Type]] {
      case OpticInfo(label, sourceType, targetType, opticType, compositeOpticType) => q"_root_.goggles.macros.interpret.OpticInfo[String]($label, ${typeStr(sourceType)}, ${typeStr(targetType)}, $opticType, $compositeOpticType)"
    }

    implicit val macroResultLiftable = Liftable[MacroResult[c.Type, c.Tree]] {
      case MacroResult(errorOrTree, infos) => q"_root_.goggles.macros.interpret.MacroResult($errorOrTree, $infos)"
    }

    q"$macroResult"
  }
}
