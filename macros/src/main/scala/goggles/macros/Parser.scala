package goggles.macros

import scala.util.Try


sealed trait ParseError
case class ParseLensRefFailed(t: Token) extends ParseError
case class ParseTargetFailed(t: Token) extends ParseError
case class ParseIndexFailed(t: Token) extends ParseError
case class ParseLensExprFailed(tokens: List[Token]) extends ParseError
case object EndOfExpr extends ParseError

object Parser {

  import AST._
  import Token._

  def parseAppliedComposedLens(tokens: List[Token]): Either[ParseError, AppliedComposedLens] = {
    tokens match {
      case Nil => Left(EndOfExpr)
      case tok :: rest => for {
        target <- parseTarget(tok)
        composedLenses <- parseComposedLens(rest)
      } yield AppliedComposedLens(target, composedLenses)

    }

  }

  def parseComposedLens(tokens: List[Token]): Either[ParseError, ComposedLens] = {

    def loop(remaining: List[Token], exprs: List[LensExpr]): Either[ParseError, ComposedLens] = {
      parseLensExpr(remaining) match {
        case (Nil, Right(lensExpr)) =>
          val h :: t = (lensExpr :: exprs).reverse
          Right(ComposedLens(h, t))

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

  def parseTarget(t: Token): Either[ParseError, Target] = {
    t match {
      case Name(name) => Right(NamedTarget(name)) 
      case Hole => Right(InterpTarget) 
      case _ => Left(ParseTargetFailed(t)) 
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
