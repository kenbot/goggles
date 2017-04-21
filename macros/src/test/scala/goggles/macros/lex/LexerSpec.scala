package goggles.macros.lex

import goggles.macros.lex.Token._
import org.specs2._


class LexerSpec extends Specification with ScalaCheck { def is =
  s2"""
    Lexer should:
      Make each opening bracket a token $openBrackets
      Make each closed bracket a token $closeBrackets
      Make each dot a token $dots
      Make each star a token $stars
      Make each single-quote a token $quotes
      Make each question mark a token $questionMarks
      Record a hole for the gaps between each fragment $holes
      Record consecutive identifier characters as a single name $identifiers
    """

  def openBrackets =
    Lexer(List("[[[")) === List(OpenBracket, OpenBracket, OpenBracket)

  def closeBrackets =
    Lexer(List("]]]")) === List(CloseBracket, CloseBracket, CloseBracket)

  def dots =
    Lexer(List("...")) === List(Dot, Dot, Dot)

  def stars =
    Lexer(List("***")) === List(Star, Star, Star)

  def quotes =
    Lexer(List("'''")) === List(Quote, Quote, Quote)

  def questionMarks =
    Lexer(List("???")) === List(Question, Question, Question)

  def holes = prop { fragments: List[String] =>
    fragments.nonEmpty ==> {
      Lexer(fragments).count(_ == Hole) === fragments.size - 1
    }
  }

  def identifiers =
    Lexer(List(".alpha123.")) === List(Dot, Name("alpha123"), Dot)

  def gibberish = prop { char: Char =>
    (!char.isLetterOrDigit && !Set('[', ']', '.', '*', '_')(char)) ==> {
      Lexer(List(char.toString)) === List(Unrecognised(char))
    }
  }
}
