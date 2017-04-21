package goggles.macros.parse

import goggles.macros.errors._

import scala.util.{Try, Success, Failure}
import scalaz.{Name => _, NonEmptyList}


sealed trait Parse[+A] {
  self =>

  def apply(list: List[Token]): Either[SyntaxError, (List[Token], A)]

  final def flatMap[B](f: A => Parse[B]): Parse[B] = new Parse[B] {
    def apply(list: List[Token]): Either[SyntaxError, (List[Token], B)] = {
      self(list).flatMap {
        case (toks, a) => f(a)(toks)
      }
    }
  }

  final def map[B](f: A => B): Parse[B] = new Parse[B] {
    def apply(list: List[Token]): Either[SyntaxError, (List[Token], B)] = {
      self(list).map {
        case (toks, a) => (toks, f(a))
      }
    }
  }

  final def many: Parse[List[A]] = new Parse[List[A]] {
    override def apply(list: List[Token]): Either[SyntaxError, (List[Token], List[A])] = {
      self(list) match {

      }
    }
  }

  final def ||[B >: A](p: => Parse[B]): Parse[B] = new Parse[B] {
    def apply(list: List[Token]): Either[SyntaxError, (List[Token], B)] = {
      self(list) match {
        case Left(_) => p(list)
        case right => right
      }
    }
  }

  private def newParser[B](f: Either[SyntaxError, (List[Token], A)] =>
                              Either[SyntaxError, (List[Token], B)]): Parse[B] = new Parse[B] {
    def apply(list: List[Token]): Either[SyntaxError, (List[Token], B)] = {
      f(self(list))
    }
  }
}

object Parse {
  def pure[A](a: A): Parse[A] = new Parse[A] {
    def apply(list: List[Token]): Either[SyntaxError, (List[Token], A)] =
      Right((list, a))
  }

  def modifyTokens(f: List[Token] => List[Token]): Parse[Unit] = new Parse[Unit] {
    def apply(list: List[Token]): Either[SyntaxError, (List[Token], Unit)] =
      Right((f(list), ()))
  }

  def nextToken: Parse[Token] = new Parse[Token] {
    def apply(list: List[Token]): Either[SyntaxError, (List[Token], Token)] = list match {
      case Nil => Left(EmptyError)
      case next :: rest => Right((rest, next))
    }
  }

  def nextChar: Parse[Char] =
    nextToken.flatMap {
      case Ch(c) => Parse.pure(c)
      case Hole => Parse.raiseError(EmptyError)
    }

  def readWhile(f: Token => Boolean): Parse[List[Token]] =
    for {
      tok <- nextToken
      if f(tok)
      list <- readWhile(f)
    } yield tok :: list

  def readName: Parse[String] = {
    readWhile(_.isChar).map(_.foldLeft("")((acc, c) => s"$c$acc"))
  }

  def raiseError(e: SyntaxError): Parse[Nothing] = new Parse[Nothing] {
    def apply(list: List[Token]): Either[SyntaxError, (List[Token], Nothing)] = {
      Left(e)
    }
  }
}

sealed trait Token {
  final def isChar = this match {
    case Ch(_) => true
    case Hole => false
  }
}
case class Ch(c: Char) extends Token
case object Hole extends Token


private[goggles] object Parser {
  import AST._


  def parseAppliedLens: Parse[AppliedLens] = Parse.nextToken.flatMap {
    case Hole => parseComposedLens.map(AppliedLens)
    case tok => Parse.raiseError(NonInterpolatedStart(tok))
  }

  def parseUnappliedLens: Parse[ComposedLens] = Parse.nextToken.flatMap {
    case Hole => Parse.modifyTokens(Ch('.') :: Hole :: _).flatMap(_ => parseComposedLens)
    case Ch(c) if !isRecognisedChar(c) => Parse.raiseError(UnrecognisedChar(c))
    case tok => Parse.raiseError(NonInterpolatedStart(tok))
  }

  private def parseComposedLens: Parse[ComposedLens] = {
    def loop(exprs: List[LensExpr]): Parse[ComposedLens] = {
      for {
        lx <- parseLensExpr
      } yield ComposedLens(NonEmptyList(h, t: _*))

      parseLensExpr match {
        case (Nil, Right(lensExpr)) =>
          val h :: t = (lensExpr :: exprs).reverse
          Right(ComposedLens(NonEmptyList(h, t: _*)))

        case (rest, Right(lensExpr)) => loop(rest, lensExpr :: exprs) 
        case (_, Left(err)) => Left(err) 
      }
    }

    loop(Nil)
  }

  def parseLensExpr: Parse[LensExpr] = {
    tokens match {
      case Ch('.') :: tok :: rest => (rest, parseLensRef(tok).right.map(RefExpr))
      case Ch('.') :: Nil  => (Nil, Left(EndingDot))
      case Ch('*') :: rest => (rest, Right(EachExpr))
      case Ch('?') :: rest => (rest, Right(OptExpr))
      case Ch('[') :: Ch(']') :: rest => (rest, Left(NoIndexSupplied))
      case Ch('[') :: tok :: Ch(']') :: rest =>
        (rest, parseIndex(tok).right.map(IndexedExpr))
      case Ch('[') :: Ch('\'') :: Name(str) :: Ch('\'') :: Ch(']') :: rest => (rest, Right(LiteralStringIndex(str)))
      case Nil => (Nil, Left(EmptyError))
      case OpenBracket :: rest if !rest.contains(CloseBracket) => (rest, Left(UnclosedOpenBracket))
      case OpenBracket :: tok :: rest => (rest, Left(InvalidIndexSupplied(tok)))
      case CloseBracket :: rest => (rest, Left(UnexpectedCloseBracket))
      case Name(n) :: rest => (rest, Left(NameWithNoDot(n)))
      case Hole :: rest => (rest, Left(InterpOpticWithNoDot))
      case Unrecognised(c) :: rest => (rest, Left(UnrecognisedChar(c)))
    }
  }

  def parseLensRef(t: Token): Either[SyntaxError, LensRef] = {
    t match {
      case Name(name) => Right(NamedLensRef(name)) 
      case Hole => Right(InterpLensRef)
      case CloseBracket => Left(UnexpectedCloseBracket)
      case tok => Left(InvalidAfterDot(tok))
    }
  }

  def parseIndex(t: Token): Either[SyntaxError, Index] = {
    t match {
      case Name(intStr) => Try(intStr.toInt) match {
        case Success(i) => Right(LiteralIntIndex(i))
        case Failure(_) => Left(VerbatimIndexNotInt(intStr))
      }
      case Hole => Right(InterpIndex)
      case CloseBracket => Left(NoIndexSupplied)
      case x => Left(InvalidIndexSupplied(x))
    }
  }

  private def isRecognisedChar(c: Char): Boolean =
    c.isLetterOrDigit || "*?.[]".contains(c)

}
