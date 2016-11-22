package goggles.macros

import scala.reflect.macros.whitebox


sealed trait Token

object Token {

  case class Name(name: String) extends Token
  case object Hole extends Token
  case object Star extends Token
  case object Dot extends Token
  case object Question extends Token
  case object OpenBracket extends Token
  case object CloseBracket extends Token
  case class Unrecognised(string: String) extends Token
}
