package goggles.macros.lex

private[goggles] sealed abstract class Token(label: String, offset: Int)

private[goggles] object Token {
  case class Name(name: String, offset: Int) extends Token(name, offset)
  case object Hole extends Token("$__", 0) // Hole will be replaced by an argument expression with its own offset
  case class Star(offset: Int) extends Token("*", offset)
  case class Dot(offset: Int) extends Token(".", offset)
  case class Question(offset: Int) extends Token("?", offset)
  case class OpenBracket(offset: Int) extends Token("[", offset)
  case class CloseBracket(offset: Int) extends Token("]", offset)
  case class Unrecognised(ch: Char, offset: Int) extends Token(ch.toString, offset)
}
