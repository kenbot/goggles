package goggles.macros.errors

import goggles.macros.interpret.OpticType
import goggles.macros.lex.Token

private[goggles] sealed trait GogglesError[+T] {
  def map[U](f: T => U): GogglesError[U]
}

sealed trait SyntaxError extends GogglesError[Nothing] {
  def map[U](f: Nothing => U): SyntaxError = this
}

case class UnrecognisedChar(char: Char) extends SyntaxError
case object EmptyError extends SyntaxError
case class NameWithNoDot(name: String) extends SyntaxError
case object InterpOpticWithNoDot extends SyntaxError
case class InvalidAfterDot(tok: Token) extends SyntaxError
case class NonInterpolatedStart(tok: Token) extends SyntaxError
case object UnexpectedCloseBracket extends SyntaxError
case object EndingDot extends SyntaxError
case object NoIndexSupplied extends SyntaxError
case class InvalidIndexSupplied(tok: Token) extends SyntaxError
case object UnclosedOpenBracket extends SyntaxError
case class VerbatimIndexNotInt(expr: String) extends SyntaxError

sealed trait MacroUserError[+T] extends GogglesError[T] {
  def map[U](f: T => U): MacroUserError[U] = this match {
    case GetterOpticRequired(x) => GetterOpticRequired(x)
    case SetterOpticRequired(x) => SetterOpticRequired(x)
    case NameNotFound(name, sourceType) => NameNotFound(name, f(sourceType))
    case NameNotAMethod(name, sourceType) => NameNotAMethod(name, f(sourceType))
    case NameHasArguments(name, sourceType) => NameHasArguments(name, f(sourceType))
    case NameHasMultiParamLists(name, sourceType) => NameHasMultiParamLists(name, f(sourceType))
    case InterpNotAnOptic(name, actualType) => InterpNotAnOptic(name, f(actualType))
    case WrongKindOfOptic(name, sourceType, targetType, from, to) => WrongKindOfOptic(name, f(sourceType), f(targetType), from, to)
    case TypesDontMatch(name, sourceType, targetType, expectedType, actualType) => TypesDontMatch(name, f(sourceType), f(targetType), f(expectedType), f(actualType))
    case ImplicitEachNotFound(name, sourceType) => ImplicitEachNotFound(name, f(sourceType))
    case ImplicitPossibleNotFound(name, sourceType) => ImplicitPossibleNotFound(name, f(sourceType))
    case ImplicitIndexNotFound(name, sourceType, indexType) => ImplicitIndexNotFound(name, f(sourceType), f(indexType))
    case CopyMethodNotFound(name, sourceType) => CopyMethodNotFound(name, f(sourceType))
    case CopyMethodNotAMethod(name, sourceType) => CopyMethodNotAMethod(name, f(sourceType))
    case CopyMethodHasMultiParamLists(name, sourceType) => CopyMethodHasMultiParamLists(name, f(sourceType))
    case CopyMethodHasNoArguments(name, sourceType) => CopyMethodHasNoArguments(name, f(sourceType))
    case CopyMethodLacksNamedArgument(name, sourceType) => CopyMethodLacksNamedArgument(name, f(sourceType))
    case CopyMethodLacksParameterDefaults(name, sourceType, argsWithNoDefault) => CopyMethodLacksParameterDefaults(name, f(sourceType), argsWithNoDefault)
  }
}

case class GetterOpticRequired(finalOpticType: OpticType) extends MacroUserError[Nothing]
case class SetterOpticRequired(finalOpticType: OpticType) extends MacroUserError[Nothing]
case class NameNotFound[T](name: String, sourceType: T) extends MacroUserError[T]
case class NameNotAMethod[T](name: String, sourceType: T) extends MacroUserError[T]
case class NameHasArguments[T](name: String, sourceType: T) extends MacroUserError[T]
case class NameHasMultiParamLists[T](name: String, sourceType: T) extends MacroUserError[T]
case class InterpNotAnOptic[T](name: String, actualType: T) extends MacroUserError[T]
case class WrongKindOfOptic[T](name: String, sourceType: T, targetType: T, from: OpticType, to: OpticType) extends MacroUserError[T]
case class TypesDontMatch[T](name: String, sourceType: T, targetType: T, expectedType: T, actualType: T) extends MacroUserError[T]
case class ImplicitEachNotFound[T](name: String, sourceType: T) extends MacroUserError[T]
case class ImplicitPossibleNotFound[T](name: String, sourceType: T) extends MacroUserError[T]
case class ImplicitIndexNotFound[T](name: String, sourceType: T, indexType: T) extends MacroUserError[T]
case class CopyMethodNotFound[T](name: String, sourceType: T) extends MacroUserError[T]
case class CopyMethodNotAMethod[T](name: String, sourceType: T) extends MacroUserError[T]
case class CopyMethodHasMultiParamLists[T](name: String, sourceType: T) extends MacroUserError[T]
case class CopyMethodHasNoArguments[T](name: String, sourceType: T) extends MacroUserError[T]
case class CopyMethodLacksNamedArgument[T](name: String, sourceType: T) extends MacroUserError[T]
case class CopyMethodLacksParameterDefaults[T](name: String, sourceType: T, argsWithNoDefault: List[String]) extends MacroUserError[T]

sealed trait MacroInternalError[+T] extends GogglesError[T] {
  def map[U](f: T => U): MacroInternalError[U] = this match {
    case UnexpectedIndexStructure(sourceType, indexType) => UnexpectedIndexStructure(f(sourceType), f(indexType))
    case UnexpectedOpticKind(actualType, numTypeArgs) => UnexpectedOpticKind(f(actualType), numTypeArgs)
    case x @ OpticInfoNotFound(_) => x
    case x @ UnexpectedEachStructure => x
    case x @ UnexpectedPossibleStructure => x
    case x @ GetVerbNotFound(_) => x
    case x @ NotEnoughArguments => x
  }
}
case class OpticInfoNotFound(label: String) extends MacroInternalError[Nothing]
case object UnexpectedEachStructure extends MacroInternalError[Nothing]
case object UnexpectedPossibleStructure extends MacroInternalError[Nothing]
case class UnexpectedIndexStructure[T](sourceType: T, indexType: T) extends MacroInternalError[T]
case class UnexpectedOpticKind[T](actualType: T, numTypeArgs: Int) extends MacroInternalError[T]
case class GetVerbNotFound(opticType: OpticType) extends MacroInternalError[Nothing]
case object NotEnoughArguments extends MacroInternalError[Nothing]