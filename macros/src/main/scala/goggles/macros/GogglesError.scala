package goggles.macros

sealed trait GogglesError[+T]

sealed trait SyntaxError extends GogglesError[Nothing]
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

sealed trait MacroUserError[+T] extends GogglesError[T]
case class NameNotFound[T](name: String, sourceType: T) extends MacroUserError[T]
case class NameNotAMethod[T](name: String, sourceType: T) extends MacroUserError[T]
case class NameHasArguments[T](name: String, sourceType: T) extends MacroUserError[T]
case class NameHasMultiParamLists[T](name: String, onType: T) extends MacroUserError[T]
case class InterpNotAnOptic[T](actualType: T) extends MacroUserError[T]
case class WrongKindOfOptic[T](from: OpticType, to: OpticType) extends MacroUserError[T]
case class TypesDontMatch[T](expectedType: T, actualType: T) extends MacroUserError[T]
case class ImplicitEachNotFound[T](sourceType: T) extends MacroUserError[T]
case class ImplicitPossibleNotFound[T](sourceType: T) extends MacroUserError[T]
case class ImplicitIndexNotFound[T](sourceType: T, indexType: T) extends MacroUserError[T]
case class CopyMethodNotFound[T](name: String, sourceType: T) extends MacroUserError[T]
case class CopyMethodNotAMethod[T](name: String, sourceType: T) extends MacroUserError[T]
case class CopyMethodHasMultiParamLists[T](name: String, sourceType: T) extends MacroUserError[T]
case class CopyMethodHasNoArguments[T](name: String, sourceType: T) extends MacroUserError[T]
case class CopyMethodLacksNamedArgument[T](name: String, sourceType: T) extends MacroUserError[T]
case class CopyMethodLacksParameterDefaults[T](name: String, sourceType: T, argsWithNoDefault: List[String]) extends MacroUserError[T]

sealed trait MacroInternalError[+T] extends GogglesError[T]
case class OpticInfoNotFound(label: String) extends MacroInternalError[Nothing]
case object UnexpectedEachStructure extends MacroInternalError[Nothing]
case object UnexpectedPossibleStructure extends MacroInternalError[Nothing]
case class UnexpectedIndexStructure[T](sourceType: T, indexType: T) extends MacroInternalError[T]
case class UnexpectedOpticKind[T](actualType: T, numTypeArgs: Int) extends MacroInternalError[T]
case class GetVerbNotFound(opticType: OpticType) extends MacroInternalError[Nothing]
case object NotEnoughArguments extends MacroInternalError[Nothing]
