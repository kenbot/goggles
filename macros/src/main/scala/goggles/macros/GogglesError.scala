package goggles.macros

import scala.reflect.api.Universe

sealed trait GogglesError

sealed trait ParsingError extends GogglesError
case class UnrecognisedChar(char: Char) extends ParsingError
case object EmptyError extends ParsingError
case class NameWithNoDot(name: String) extends ParsingError
case object InterpOpticWithNoDot extends ParsingError
case class InvalidAfterDot(tok: Token) extends ParsingError
case class NonInterpolatedStart(tok: Token) extends ParsingError
case object UnexpectedCloseBracket extends ParsingError
case object EndingDot extends ParsingError
case object NoIndexSupplied extends ParsingError
case class InvalidIndexSupplied(tok: Token) extends ParsingError
case object UnclosedOpenBracket extends ParsingError
case class VerbatimIndexNotPositiveInt(expr: String) extends ParsingError


sealed trait InterpretError extends GogglesError
case class NameNotFound(name: String, sourceType: Universe#Type) extends InterpretError
case class NameHasArguments(name: String, sourceType: Universe#Type) extends InterpretError
case class NameHasMultipleParameterLists(name: String, onType: Universe#Type) extends InterpretError
case class InterpNotAnOptic(name: String, actualType: Universe#Type) extends InterpretError
case class WrongKindOfOptic(from: OpticType, to: OpticType) extends InterpretError
case class TypesDontMatch(sourceType: Universe#Type, targetType: Universe#Type) extends InterpretError
case class ImplicitEachNotFound(sourceType: Universe#Type) extends InterpretError
case class ImplicitPossibleNotFound(sourceType: Universe#Type) extends InterpretError
case class ImplicitIndexNotFound(sourceType: Universe#Type) extends InterpretError
case class ImplicitEachNotFoundForAdtSubClass(subType: Universe#Type, superType: Universe#Type) extends InterpretError
case class ImplicitPossibleNotFoundForAdtSubClass(subType: Universe#Type, superType: Universe#Type) extends InterpretError
case class ImplicitIndexNotFoundForAdtSubClass(subType: Universe#Type, superType: Universe#Type) extends InterpretError
case class CopyMethodMissing(name: String, sourceType: Universe#Type) extends InterpretError
case class CopyMethodHasMultiParamLists(name: String, sourceType: Universe#Type) extends InterpretError
case class CopyMethodLacksNamedArgument(name: String, sourceType: Universe#Type) extends InterpretError
case class CopyMethodLacksParameterDefaults(name: String, sourceType: Universe#Type) extends InterpretError

sealed trait InternalError extends GogglesError
case object ParseInfoNotFound extends GogglesError
case object UnexpectedEachStructure extends InternalError
case object UnexpectedPossibleStructure extends InternalError
case object UnexpectedIndexStructure extends InternalError
case class UnexpectedOpticKind(actualType: Universe#Type, numTypeArgs: Int) extends InternalError
case class GetVerbNotFound(opticType: OpticType) extends InternalError
case object NotEnoughArguments extends InternalError

