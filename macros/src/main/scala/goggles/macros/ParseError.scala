package goggles.macros

sealed trait ParseError
case class ParseLensRefFailed(t: Token) extends ParseError
case class ParseTargetFailed(t: Token) extends ParseError
case class ParseIndexFailed(t: Token) extends ParseError
case class ParseLensExprFailed(tokens: List[Token]) extends ParseError
case class MissingInputType(name: String) extends ParseError
case object EndOfExpr extends ParseError
case class GetNotAllowed(resultType: String) extends ParseError
case class SetNotAllowed(resultType: String) extends ParseError
case class InvalidGetter(resultType: String) extends ParseError
case class InvalidSetter(resultType: String) extends ParseError
case class InvalidOpticComposition(o1: OpticType, o2: OpticType) extends ParseError
case class OpticTypecheckFailed(expr: String) extends ParseError
case class ImplicitEachNotFound(inputType: String) extends ParseError
case class ImplicitPossibleNotFound(inputType: String) extends ParseError
case class ImplicitIndexNotFound(inputType: String) extends ParseError
case class OptionNotFound(inputType: String) extends ParseError
case object NoLensesProvided extends ParseError
case class InternalError(reason: String) extends ParseError


