package goggles.macros

import scala.util.Try

import scalaz._, Scalaz._

object Parser {
  import AST._
  import Token._

  def parseAppliedLens(tokens: List[Token]): Either[ParseError, AppliedLens] = {
    tokens match {
      case Nil => Left(EndOfExpr)
      case Hole :: rest => parseComposedLens(rest).map(AppliedLens)
      case x :: _ => Left(ParseTargetFailed(x))
    }
  }

  def parseComposedLens(tokens: List[Token]): Either[ParseError, ComposedLens] = {

    def loop(remaining: List[Token], exprs: List[LensExpr]): Either[ParseError, ComposedLens] = {
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

  def parseLensExpr(tokens: List[Token]): (List[Token], Either[ParseError, LensExpr]) = {
    tokens match {
      case Nil => (Nil, Left(EndOfExpr))
      case Dot :: tok :: rest => (rest, parseLensRef(tok).map(RefExpr))
      case Star :: rest => (rest, Right(EachExpr))
      case Question :: rest => (rest, Right(OptExpr))
      case OpenBracket :: tok :: CloseBracket :: rest => 
        (rest, parseIndex(tok).map(IndexedExpr))
      case rest => (rest, Left(ParseLensExprFailed(rest)))

    }
  }

  def parseLensRef(t: Token): Either[ParseError, LensRef] = {
    t match {
      case Name(name) => Right(NamedLensRef(name)) 
      case Hole => Right(InterpLensRef) 
      case _ => Left(ParseLensRefFailed(t)) 
    }
  }

  def parseIndex(t: Token): Either[ParseError, Index] = {
    def err = ParseIndexFailed(t)
    t match {
      case Name(intStr) => Try(LiteralIndex(intStr.toInt)).
        toEither.left.map(_ => err) 
      case Hole => Right(InterpIndex)
      case _ => Left(err) 
    }
  }
}
