package goggles.macros


object Lexer {
  import Token._

  def apply(fragments: List[String]): List[Token] = {
    fragments match {
      case Nil => Nil
      case s :: ss => lex(s) ::: ss.flatMap(s => Hole :: lex(s)) 
    }
  }

  private def lex(str: String): List[Token] = {

    def isIdentifierChar(c: Char): Boolean = 
      c.isLetterOrDigit || c == '_'

    def mkName(chars: List[Char]): List[Name] = chars match {
      case Nil => Nil
      case _ => Name(new String(chars.reverse.toArray)) :: Nil
    }

    def loop(nameSoFar: List[Char], rest: List[Char], tokens: List[Token]): List[Token] = {
      rest match {
        case Nil => mkName(nameSoFar) ::: tokens
        case c :: tail if isIdentifierChar(c) => loop(c :: nameSoFar, tail, tokens)
        case '.' :: tail => loop(Nil, tail, Dot :: mkName(nameSoFar) ::: tokens)
        case '*' :: tail => loop(Nil, tail, Star :: mkName(nameSoFar) ::: tokens) 
        case '?' :: tail => loop(Nil, tail, Question :: mkName(nameSoFar) ::: tokens) 
        case '[' :: tail => loop(Nil, tail, OpenBracket :: mkName(nameSoFar) ::: tokens) 
        case ']' :: tail => loop(Nil, tail, CloseBracket :: mkName(nameSoFar) ::: tokens) 
        case x => Unrecognised(x.toString) :: tokens
      }
    }

    loop(Nil, str.toList, Nil).reverse
  }
}
