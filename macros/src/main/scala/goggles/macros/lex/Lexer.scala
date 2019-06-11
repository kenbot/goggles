package goggles.macros.lex

private[goggles] object Lexer {

  def apply(fragmentsWithOffset: List[(String, Int)]): List[Token] = {
    fragmentsWithOffset match {
      case Nil => Nil
      case (s0, offset0) :: ss => lexFragment(s0, offset0) ::: ss.flatMap { 
        case (s, offset) => Token.Hole :: lexFragment(s, offset) 
      }
    }
  }

  private def lexFragment(fragment: String, offset0: Int): List[Token] = {

    def isIdentifierChar(c: Char): Boolean = 
      c.isLetterOrDigit || c == '_'

    def loop(nameSoFar: List[Char], rest: List[Char], tokens: List[Token], offset: Int): List[Token] = {
      def mkName: List[Token.Name] = nameSoFar match {
        case Nil => Nil
        case _ => Token.Name(new String(nameSoFar.reverse.toArray), offset) :: Nil
      }
      
      def nameLen = nameSoFar.length

      rest match {
        case Nil => mkName ::: tokens
        case c :: tail if isIdentifierChar(c) => loop(c :: nameSoFar, tail, tokens, offset)
        case '.' :: tail => loop(Nil, tail, Token.Dot(offset + nameLen) :: mkName ::: tokens, offset + nameLen + 1)
        case '*' :: tail => loop(Nil, tail, Token.Star(offset + nameLen) :: mkName ::: tokens, offset + nameLen + 1)
        case '?' :: tail => loop(Nil, tail, Token.Question(offset + nameLen) :: mkName ::: tokens, offset + nameLen + 1)
        case '[' :: tail => loop(Nil, tail, Token.OpenBracket(offset + nameLen) :: mkName ::: tokens, offset + nameLen + 1)
        case ']' :: tail => loop(Nil, tail, Token.CloseBracket(offset + nameLen) :: mkName ::: tokens, offset + nameLen + 1)
        case x :: tail => loop(Nil, tail, Token.Unrecognised(x, offset + nameLen) :: tokens, offset + nameLen + 1)
      }
    }

    loop(Nil, fragment.toList, Nil, offset0).reverse
  }
}
