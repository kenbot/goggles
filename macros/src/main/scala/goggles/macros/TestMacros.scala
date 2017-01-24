package goggles.macros

import scala.reflect.macros.whitebox


object TestMacros {

  def getImpl(c: whitebox.Context)(args: c.Expr[Any]*): c.Tree = {
    handleResult(c)(MacroInterpreter.getImpl(c)(args: _*)._1)
  }

  def setImpl(c: whitebox.Context)(args: c.Expr[Any]*): c.Tree = {
    handleResult(c)(MacroInterpreter.setImpl(c)(args: _*)._1)
  }

  def lensImpl(c: whitebox.Context)(args: c.Expr[Any]*): c.Tree = {
    handleResult(c)(MacroInterpreter.lensImpl(c)(args: _*)._1)
  }

  private def handleResult(c: whitebox.Context)(
    result: Either[GogglesError[c.Type], c.Tree]): c.Tree = {

    import OpticType._
    import c.universe._

    implicit val opticTypeLiftable = Liftable[OpticType] {
      case FoldType => q"_root_.goggles.macros.OpticType.FoldType"
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
      case UnrecognisedChar(char) => q"_root_.goggles.macros.UnrecognisedChar($char)"
      case EmptyError => q"_root_.goggles.macros.EmptyError"
      case NameWithNoDot(name) => q"_root_.goggles.macros.NameWithNoDot($name)"
      case InterpOpticWithNoDot => q"_root_.goggles.macros.InterpOpticWithNoDot"
      case InvalidAfterDot(tok) => q"_root_.goggles.macros.InvalidAfterDot($tok)"
      case NonInterpolatedStart(tok) => q"_root_.goggles.macros.NonInterpolatedStart($tok)"
      case UnexpectedCloseBracket => q"_root_.goggles.macros.UnexpectedCloseBracket"
      case EndingDot => q"_root_.goggles.macros.EndingDot"
      case NoIndexSupplied => q"_root_.goggles.macros.NoIndexSupplied"
      case InvalidIndexSupplied(tok) => q"_root_.goggles.macros.InvalidIndexSupplied($tok)"
      case UnclosedOpenBracket => q"_root_.goggles.macros.UnclosedOpenBracket"
      case VerbatimIndexNotInt(expr) => q"_root_.goggles.macros.VerbatimIndexNotPositiveInt($expr)"
      case NameNotFound(name, sourceType) => q"_root_.goggles.macros.NameNotFound[String]($name, ${typeStr(sourceType)})"
      case NameNotAMethod(name, sourceType) => q"_root_.goggles.macros.NameNotAMethod[String]($name, ${typeStr(sourceType)})"
      case NameHasArguments(name, sourceType) => q"_root_.goggles.macros.NameHasArguments[String]($name, ${typeStr(sourceType)})"
      case NameHasMultiParamLists(name, onType) => q"_root_.goggles.macros.NameHasMultiParamLists[String]($name, ${typeStr(onType)})"
      case InterpNotAnOptic(actualType) => q"_root_.goggles.macros.InterpNotAnOptic[String](${typeStr(actualType)})"
      case WrongKindOfOptic(from, to) => q"_root_.goggles.macros.WrongKindOfOptic($from, $to)"
      case TypesDontMatch(expectedType, actualType) => q"_root_.goggles.macros.TypesDontMatch[String](${typeStr(expectedType)}, ${typeStr(actualType)})"
      case ImplicitEachNotFound(sourceType) => q"_root_.goggles.macros.ImplicitEachNotFound[String](${typeStr(sourceType)})"
      case ImplicitPossibleNotFound(sourceType) => q"_root_.goggles.macros.ImplicitPossibleNotFound[String](${typeStr(sourceType)})"
      case ImplicitIndexNotFound(sourceType, indexType) => q"_root_.goggles.macros.ImplicitIndexNotFound[String](${typeStr(sourceType)}, ${typeStr(indexType)})"
      case CopyMethodNotFound(name, sourceType) => q"_root_.goggles.macros.CopyMethodNotFound[String]($name, ${typeStr(sourceType)})"
      case CopyMethodNotAMethod(name, sourceType) => q"_root_.goggles.macros.CopyMethodNotAMethod[String]($name, ${typeStr(sourceType)})"
      case CopyMethodHasMultiParamLists(name, sourceType) => q"_root_.goggles.macros.CopyMethodHasMultiParamLists[String]($name, ${typeStr(sourceType)})"
      case CopyMethodHasNoArguments(name, sourceType) => q"_root_.goggles.macros.CopyMethodHasNoArguments[String]($name, ${typeStr(sourceType)})"
      case CopyMethodLacksNamedArgument(name, sourceType) => q"_root_.goggles.macros.CopyMethodLacksNamedArgument[String]($name, ${typeStr(sourceType)})"
      case CopyMethodLacksParameterDefaults(name, sourceType, argsWithNoDefault) => q"_root_.goggles.macros.CopyMethodLacksParameterDefaults[String]($name, ${typeStr(sourceType)}, $argsWithNoDefault)"
      case ParseInfoNotFound(label) => q"_root_.goggles.macros.ParseInfoNotFound($label)"
      case UnexpectedEachStructure => q"_root_.goggles.macros.UnexpectedEachStructure"
      case UnexpectedPossibleStructure => q"_root_.goggles.macros.UnexpectedPossibleStructure"
      case UnexpectedIndexStructure(sourceType, indexType) => q"_root_.goggles.macros.UnexpectedIndexStructure[String](${typeStr(sourceType)}, ${typeStr(indexType)})"
      case UnexpectedOpticKind(actualType, numTypeArgs) => q"_root_.goggles.macros.UnexpectedOpticKind[String](${typeStr(actualType)}, $numTypeArgs)"
      case GetVerbNotFound(opticType) => q"_root_.goggles.macros.GetVerbNotFound($opticType)"
      case NotEnoughArguments => q"_root_.goggles.macros.NotEnoughArguments"
    }

    result match {
      case Right(tree) => q"_root_.scala.Right($tree)"
      case Left(err) => q"_root_.scala.Left($err)"
    }
  }

}
