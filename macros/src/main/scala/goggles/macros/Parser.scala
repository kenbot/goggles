package goggles.macros

import scala.util.Try

import scalaz._, Scalaz._

object Parser {
  import AST._
  import Token._


  def parseAppliedLens(tokens: List[Token]): Either[SyntaxError, AppliedLens] = {
    tokens match {
      case Nil => Left(EmptyError)
      case Hole :: rest => parseComposedLens(rest).map(AppliedLens)
      case Unrecognised(c) :: _ => Left(UnrecognisedChar(c))
      case tok :: _ => Left(NonInterpolatedStart(tok))
    }
  }

  def parseUnappliedLens(tokens: List[Token]): Either[SyntaxError, ComposedLens] = {
    tokens match {
      case Nil => Left(EmptyError)
      case Hole :: rest => parseComposedLens(Dot :: Hole :: rest)
      case Unrecognised(c) :: _ => Left(UnrecognisedChar(c))
      case tok :: _ => Left(NonInterpolatedStart(tok))
    }
  }

  private def parseComposedLens(tokens: List[Token]): Either[SyntaxError, ComposedLens] = {

    def loop(remaining: List[Token], exprs: List[LensExpr]): Either[SyntaxError, ComposedLens] = {
      parseLensExpr(remaining) match {
        case (Nil, Right(lensExpr)) =>
          val h :: t = (lensExpr :: exprs).reverse
          Right(ComposedLens(NonEmptyList(h, t: _*)))

        case (rest, Right(lensExpr)) => loop(rest, lensExpr :: exprs) 
        case (_, Left(err)) => Left(err) 
      }
    }

    loop(tokens, Nil)
  }

  def parseLensExpr(tokens: List[Token]): (List[Token], Either[SyntaxError, LensExpr]) = {
    tokens match {
      case Dot :: tok :: rest => (rest, parseLensRef(tok).map(RefExpr))
      case Dot :: Nil  => (Nil, Left(EndingDot))
      case Star :: rest => (rest, Right(EachExpr))
      case Question :: rest => (rest, Right(OptExpr))
      case OpenBracket :: CloseBracket :: rest => (rest, Left(NoIndexSupplied))
      case OpenBracket :: tok :: CloseBracket :: rest => 
        (rest, parseIndex(tok).map(IndexedExpr))
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
      case Name(intStr) => Try(intStr.toInt).collect {
        case i => LiteralIndex(i)
      }.toEither.left.map(_ => VerbatimIndexNotInt(intStr))
      case Hole => Right(InterpIndex)
      case CloseBracket => Left(NoIndexSupplied)
      case x => Left(InvalidIndexSupplied(x))
    }
  }
}
