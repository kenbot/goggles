package goggles.macros.lex
import goggles.macros.At

private[goggles] sealed abstract class Token(val label: String) {
  def at(offset: Int): At[Token] = 
    At(this, offset)
}


private[goggles] object Token {
  case class Name(name: String) extends Token(name)
  case object Hole extends Token("$__")
  case object Star extends Token("*")
  case object Dot extends Token(".")
  case object Question extends Token("?")
  case object OpenBracket extends Token("[")
  case object CloseBracket extends Token("]")
  case class Unrecognised(ch: Char) extends Token(ch.toString)
}
