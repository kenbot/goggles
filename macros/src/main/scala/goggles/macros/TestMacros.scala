package goggles.macros

import goggles.macros.errors._

import scala.reflect.macros.whitebox


object TestMacros {

  def getImpl(c: whitebox.Context)(args: c.Expr[Any]*): c.Tree = {
    handleResult(c)(MacroInterpreter.getImpl(c)(args: _*).errorOrTree)
  }

  def setImpl(c: whitebox.Context)(args: c.Expr[Any]*): c.Tree = {
    handleResult(c)(MacroInterpreter.setImpl(c)(args: _*).errorOrTree)
  }

  def lensImpl(c: whitebox.Context)(args: c.Expr[Any]*): c.Tree = {
    handleResult(c)(MacroInterpreter.lensImpl(c)(args: _*).errorOrTree)
  }

  private def handleResult(c: whitebox.Context)(
    result: Either[GogglesError[c.Type], c.Tree]): c.Tree = {

    import OpticType._
    import c.universe._


    implicit val opticTypeLiftable = Liftable[OpticType] {
      case FoldType => q"_root_.goggles.OpticType.FoldType"
      case GetterType => q"_root_.goggles.macros.OpticType.GetterType"
      case SetterType => q"_root_.goggles.macros.OpticType.SetterType"
      case TraversalType => q"_root_.goggles.macros.OpticType.TraversalType"
      case OptionalType => q"_root_.goggles.macros.OpticType.OptionalType"
      case PrismType => q"_root_.goggles.macros.OpticType.PrismType"
      case LensType => q"_root_.goggles.macros.OpticType.LensType"
      case IsoType =>  q"_root_.goggles.macros.OpticType.IsoType"
    }

    implicit val tokenLiftable = Liftable[Token] {
      case Token.Name(name) => q"_root_.goggles.macros.Token.Name($name)"
      case Token.Dot => q"_root_.goggles.macros.Token.Dot"
      case Token.OpenBracket => q"_root_.goggles.macros.Token.OpenBracket"
      case Token.CloseBracket => q"_root_.goggles.macros.Token.CloseBracket"
      case Token.Star => q"_root_.goggles.macros.Token.Star"
      case Token.Question => q"_root_.goggles.macros.Token.Question"
      case Token.Hole => q"_root_.goggles.macros.Token.Hole"
      case Token.Unrecognised(ch) => q"_root_.goggles.macros.Token.Unrecognised($ch)"
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

    result match {
      case Right(tree) => q"_root_.scala.Right($tree)"
      case Left(err) => q"_root_.scala.Left($err)"
    }
  }

}
