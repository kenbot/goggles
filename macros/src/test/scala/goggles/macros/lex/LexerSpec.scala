package goggles.macros.lex

import org.specs2._


class LexerSpec extends Specification with ScalaCheck { def is =
  s2"""
    Lexer should:
      Make each opening bracket a token $openBrackets
      Make each closed bracket a token $closeBrackets
      Make each dot a token $dots
      Make each star a token $stars
      Chop off a name when a star is encountered $nameThenStar
      Chop off a name when an open bracket is encountered $nameThenOpenBracket
      Chop off a name when a question mark is encountered $nameThenQuestion
      Make each question mark a token $questionMarks
      Record a hole for the gaps between each fragment $holes
      Record consecutive identifier characters as a single name $identifiers
    """

  def openBrackets =
    Lexer(List(("[[[", 0))) === List(Token.OpenBracket(0), Token.OpenBracket(1), Token.OpenBracket(2))

  def closeBrackets =
    Lexer(List(("]]]", 0))) === List(Token.CloseBracket(0), Token.CloseBracket(1), Token.CloseBracket(2))

  def dots =
    Lexer(List(("...", 0))) === List(Token.Dot(0), Token.Dot(1), Token.Dot(2))

  def stars =
    Lexer(List(("***", 0))) === List(Token.Star(0), Token.Star(1), Token.Star(2))

  def nameThenStar =
    Lexer(List(("aaa*", 0))) === List(Token.Name("aaa",0), Token.Star(3))

  def nameThenQuestion =
    Lexer(List(("aaa?", 0))) === List(Token.Name("aaa",0), Token.Question(3))

  def nameThenOpenBracket =
    Lexer(List(("aaa[", 0))) === List(Token.Name("aaa",0), Token.OpenBracket(3))

  def questionMarks =
    Lexer(List(("???",0))) === List(Token.Question(0), Token.Question(1), Token.Question(2))

  def holes = prop { fragments: List[(String, Int)] =>
    fragments.nonEmpty ==> {
      Lexer(fragments).count(_ == Token.Hole) === fragments.size - 1
    }
  }

  def identifiers =
    Lexer(List((".alpha123.", 0))) === List(Token.Dot(0), Token.Name("alpha123", 1), Token.Dot(9))

  def gibberish = prop { char: Char =>
    (!char.isLetterOrDigit && !Set('[', ']', '.', '*', '_')(char)) ==> {
      Lexer(List((char.toString, 0))) === List(Token.Unrecognised(char, 0))
    }
  }
}
