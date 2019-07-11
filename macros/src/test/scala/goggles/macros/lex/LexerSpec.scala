package goggles.macros.lex

import goggles.macros.At

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
      Record consecutive identifier characters as a single name $identifiers
    """

  def openBrackets =
    Lexer(List(Fragment.Verbatim("[[[", 0))) === List(At(Token.OpenBracket, 0), At(Token.OpenBracket, 1), At(Token.OpenBracket, 2))

  def closeBrackets =
    Lexer(List(Fragment.Verbatim("]]]", 0))) === List(At(Token.CloseBracket, 0), At(Token.CloseBracket, 1), At(Token.CloseBracket, 2))

  def dots =
    Lexer(List(Fragment.Verbatim("...", 0))) === List(At(Token.Dot, 0), At(Token.Dot, 1), At(Token.Dot, 2))

  def stars =
    Lexer(List(Fragment.Verbatim("***", 0))) === List(At(Token.Star, 0), At(Token.Star, 1), At(Token.Star, 2))

  def nameThenStar =
    Lexer(List(Fragment.Verbatim("aaa*", 0))) === List(At(Token.Name("aaa"), 0), At(Token.Star, 3))

  def nameThenQuestion =
    Lexer(List(Fragment.Verbatim("aaa?", 0))) === List(At(Token.Name("aaa"), 0), At(Token.Question, 3))

  def nameThenOpenBracket =
    Lexer(List(Fragment.Verbatim("aaa[", 0))) === List(At(Token.Name("aaa"), 0), At(Token.OpenBracket, 3))

  def questionMarks =
    Lexer(List(Fragment.Verbatim("???",0))) === List(At(Token.Question, 0), At(Token.Question, 1), At(Token.Question, 2))

  def identifiers =
    Lexer(List(Fragment.Verbatim(".alpha123.", 0))) === List(At(Token.Dot, 0), At(Token.Name("alpha123"), 1), At(Token.Dot, 9))

  def gibberish = prop { char: Char =>
    (!char.isLetterOrDigit && !Set('[', ']', '.', '*', '_')(char)) ==> {
      Lexer(List(Fragment.Verbatim(char.toString, 0))) === List(Token.Unrecognised(char).at(0))
    }
  }
}
