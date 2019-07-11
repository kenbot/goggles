package goggles.macros.lex
import goggles.macros.At

private[goggles] object Lexer {

  def apply(fragments: List[Fragment]): List[At[Token]] = {
    fragments.flatMap {
      case Fragment.Verbatim(text, offset) => lexFragment(text, offset)
      case Fragment.Argument(offset) => List(Token.Hole.at(offset))
    }
  }

  private def lexFragment(fragment: String, relativeOffset0: Int): List[At[Token]] = {

    def isIdentifierChar(c: Char): Boolean = 
      c.isLetterOrDigit || c == '_'

    def loop(nameSoFar: List[Char], rest: List[Char], tokens: List[At[Token]], offset: Int): List[At[Token]] = {
      def mkName: List[At[Token]] = nameSoFar match {
        case Nil => Nil
        case _ => Token.Name(new String(nameSoFar.reverse.toArray)).at(offset) :: Nil
      }
      
      def nameLen = nameSoFar.length

      rest match {
        case Nil => mkName ::: tokens
        case c :: tail if isIdentifierChar(c) => loop(c :: nameSoFar, tail, tokens, offset)
        case '.' :: tail => loop(Nil, tail, Token.Dot.at(offset + nameLen) :: mkName ::: tokens, offset + nameLen + 1)
        case '*' :: tail => loop(Nil, tail, Token.Star.at(offset + nameLen) :: mkName ::: tokens, offset + nameLen + 1)
        case '?' :: tail => loop(Nil, tail, Token.Question.at(offset + nameLen) :: mkName ::: tokens, offset + nameLen + 1)
        case '[' :: tail => loop(Nil, tail, Token.OpenBracket.at(offset + nameLen) :: mkName ::: tokens, offset + nameLen + 1)
        case ']' :: tail => loop(Nil, tail, Token.CloseBracket.at(offset + nameLen) :: mkName ::: tokens, offset + nameLen + 1)
        case x :: tail => loop(Nil, tail, Token.Unrecognised(x).at(offset + nameLen) :: tokens, offset + nameLen + 1)
      }
    }

    loop(Nil, fragment.toList, Nil, relativeOffset0).reverse
  }
}
