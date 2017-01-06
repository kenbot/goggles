package goggles.macros

import scala.util.Try

import scalaz._, Scalaz._

object Parser {
  import AST._
  import Token._

  def parseAppliedLens(tokens: List[Token]): Either[GogglesError, AppliedLens] = {
    tokens match {
      case Nil => Left(EmptyError)
      case Hole :: rest => parseComposedLens(rest).map(AppliedLens)
      case tok :: _ => Left(NonInterpolatedStart(tok))
    }
  }

  def parseUnappliedLens(tokens: List[Token]): Either[GogglesError, ComposedLens] = {
    tokens match {
      case Nil => Left(EmptyError)
      case Hole :: rest => parseComposedLens(Dot :: Hole :: rest)
      case tok :: _ => Left(NonInterpolatedStart(tok))
    }
  }

  private def parseComposedLens(tokens: List[Token]): Either[GogglesError, ComposedLens] = {

    def loop(remaining: List[Token], exprs: List[LensExpr]): Either[GogglesError, ComposedLens] = {
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

  def parseLensExpr(tokens: List[Token]): (List[Token], Either[GogglesError, LensExpr]) = {
    tokens match {
      case Dot :: tok :: rest => (rest, parseLensRef(tok).map(RefExpr))
      case Dot :: Nil  => (Nil, Left(EndingDot))
      case Star :: rest => (rest, Right(EachExpr))
      case Question :: rest => (rest, Right(OptExpr))
      case OpenBracket :: tok :: CloseBracket :: rest => 
        (rest, parseIndex(tok).map(IndexedExpr))
      case Nil => (Nil, Left(EmptyError))
      case OpenBracket :: rest => (rest, Left(UnclosedOpenBracket))
      case CloseBracket :: rest => (rest, Left(UnexpectedCloseBracket))
      case Name(n) :: rest => (rest, Left(NameWithNoDot(n)))
      case Hole :: rest => (rest, Left(InterpOpticWithNoDot))
      case Unrecognised(c) :: rest => (rest, Left(UnrecognisedChar(c)))
    }
  }

  def parseLensRef(t: Token): Either[GogglesError, LensRef] = {
    t match {
      case Name(name) => Right(NamedLensRef(name)) 
      case Hole => Right(InterpLensRef) 
      case tok => Left(InvalidAfterDot(tok))
    }
  }

  def parseIndex(t: Token): Either[GogglesError, Index] = {
    t match {
      case Name(intStr) => Try(intStr.toInt).collect {
        case i => LiteralIndex(i)
      }.toEither.left.map(_ => VerbatimIndexNotPositiveInt(intStr))
      case Hole => Right(InterpIndex)
      case CloseBracket => Left(NoIndexSupplied)
      case x => Left(InvalidIndexSupplied(x))
    }
  }
}
