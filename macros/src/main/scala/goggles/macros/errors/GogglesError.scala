package goggles.macros.errors

import goggles.macros.interpret.OpticType
import goggles.macros.lex.Token

private[goggles] sealed trait GogglesError[+T] {
  def map[U](f: T => U): GogglesError[U]

  def at(offset: Int): ErrorAt[T] = 
    ErrorAt(this, offset)
}

sealed trait SyntaxError extends GogglesError[Nothing] {
  def map[U](f: Nothing => U): SyntaxError = this
}

object SyntaxError {
  case class UnrecognisedChar(char: Char) extends SyntaxError
  case object EmptyError extends SyntaxError
  case class NameWithNoDot(name: String) extends SyntaxError
  case object InterpOpticWithNoDot extends SyntaxError
  case class InvalidAfterDot(token: Token) extends SyntaxError
  case class NonInterpolatedStart(token: Token) extends SyntaxError
  case object UnexpectedCloseBracket extends SyntaxError
  case object EndingDot extends SyntaxError
  case object NoIndexSupplied extends SyntaxError
  case class InvalidIndexSupplied(token: Token) extends SyntaxError
  case object UnclosedOpenBracket extends SyntaxError
  case class VerbatimIndexNotInt(expr: String) extends SyntaxError
}

sealed trait UserError[+T] extends GogglesError[T] {
  import UserError._

  def map[U](f: T => U): UserError[U] = this match {
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

object UserError {
  case class GetterOpticRequired(finalOpticType: OpticType) extends UserError[Nothing]
  case class SetterOpticRequired(finalOpticType: OpticType) extends UserError[Nothing]
  case class NameNotFound[T](name: String, sourceType: T) extends UserError[T]
  case class NameNotAMethod[T](name: String, sourceType: T) extends UserError[T]
  case class NameHasArguments[T](name: String, sourceType: T) extends UserError[T]
  case class NameHasMultiParamLists[T](name: String, sourceType: T) extends UserError[T]
  case class InterpNotAnOptic[T](name: String, actualType: T) extends UserError[T]
  case class WrongKindOfOptic[T](name: String, sourceType: T, targetType: T, from: OpticType, to: OpticType) extends UserError[T]
  case class TypesDontMatch[T](name: String, sourceType: T, targetType: T, expectedType: T, actualType: T) extends UserError[T]
  case class ImplicitEachNotFound[T](name: String, sourceType: T) extends UserError[T]
  case class ImplicitPossibleNotFound[T](name: String, sourceType: T) extends UserError[T]
  case class ImplicitIndexNotFound[T](name: String, sourceType: T, indexType: T) extends UserError[T]
  case class CopyMethodNotFound[T](name: String, sourceType: T) extends UserError[T]
  case class CopyMethodNotAMethod[T](name: String, sourceType: T) extends UserError[T]
  case class CopyMethodHasMultiParamLists[T](name: String, sourceType: T) extends UserError[T]
  case class CopyMethodHasNoArguments[T](name: String, sourceType: T) extends UserError[T]
  case class CopyMethodLacksNamedArgument[T](name: String, sourceType: T) extends UserError[T]
  case class CopyMethodLacksParameterDefaults[T](name: String, sourceType: T, argsWithNoDefault: List[String]) extends UserError[T]
}

sealed trait InternalError[+T] extends GogglesError[T] {
  import InternalError._
  def map[U](f: T => U): InternalError[U] = this match {
    case UnexpectedIndexStructure(sourceType, indexType) => UnexpectedIndexStructure(f(sourceType), f(indexType))
    case UnexpectedOpticKind(actualType, numTypeArgs) => UnexpectedOpticKind(f(actualType), numTypeArgs)
    case x @ OpticInfoNotFound(_) => x
    case x @ UnexpectedEachStructure => x
    case x @ UnexpectedPossibleStructure => x
    case x @ GetVerbNotFound(_) => x
    case x @ NotEnoughArguments => x
  }
}

object InternalError {
  case class OpticInfoNotFound(label: String) extends InternalError[Nothing]
  case object UnexpectedEachStructure extends InternalError[Nothing]
  case object UnexpectedPossibleStructure extends InternalError[Nothing]
  case class UnexpectedIndexStructure[T](sourceType: T, indexType: T) extends InternalError[T]
  case class UnexpectedOpticKind[T](actualType: T, numTypeArgs: Int) extends InternalError[T]
  case class GetVerbNotFound(opticType: OpticType) extends InternalError[Nothing]
  case object NotEnoughArguments extends InternalError[Nothing]
}