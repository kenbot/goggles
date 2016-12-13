package goggles.macros

sealed trait ParseError
case class ParseLensRefFailed(t: Token) extends ParseError
case class ParseTargetFailed(t: Token) extends ParseError
case class ParseIndexFailed(t: Token) extends ParseError
case class ParseLensExprFailed(tokens: List[Token]) extends ParseError
case class NamedLensWithoutType(name: String) extends ParseError
case object EndOfExpr extends ParseError
case class GetNotAllowed(resultType: String) extends ParseError
case object NoLensesProvided extends ParseError
case class InternalError(reason: String) extends ParseError


