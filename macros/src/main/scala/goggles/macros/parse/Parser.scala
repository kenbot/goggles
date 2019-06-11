package goggles.macros.parse

import goggles.macros.lex.Token
import goggles.macros.errors.SyntaxError

import scala.util.{Try, Success, Failure}

private[goggles] object Parser {

  def parseAppliedLens(tokens: List[Token]): Either[SyntaxError, AST] = {
    tokens match {
      case Nil => Left(SyntaxError.EmptyError)
      case Token.Hole :: rest => parseAST(rest)
      case Token.Unrecognised(c, _) :: _ => Left(SyntaxError.UnrecognisedChar(c))
      case tok :: _ => Left(SyntaxError.NonInterpolatedStart(tok))
    }
  }

  def parseUnappliedLens(tokens: List[Token]): Either[SyntaxError, AST] = {
    tokens match {
      case Nil => Left(SyntaxError.EmptyError)
      case Token.Hole :: rest => parseAST(Token.Dot(-999) :: Token.Hole :: rest)
      case Token.Unrecognised(c, _) :: _ => Left(SyntaxError.UnrecognisedChar(c))
      case tok :: _ => Left(SyntaxError.NonInterpolatedStart(tok))
    }
  }

  private def parseAST(tokens: List[Token]): Either[SyntaxError, AST] = {

    def loop(remaining: List[Token], exprs: List[LensExpr]): Either[SyntaxError, AST] = {
      parseLensExpr(remaining) match {
        case (Nil, Right(lensExpr)) =>
          val list = (lensExpr :: exprs).reverse
          Right(AST(list.head, list.tail))

        case (rest, Right(lensExpr)) => loop(rest, lensExpr :: exprs) 
        case (_, Left(err)) => Left(err) 
      }
    }

    loop(tokens, Nil)
  }

  def parseLensExpr(tokens: List[Token]): (List[Token], Either[SyntaxError, LensExpr]) = {
    tokens match {
      case Token.Dot(_) :: tok :: rest => (rest, parseLensRef(tok).right.map(LensExpr.Ref))
      case Token.Dot(_) :: Nil  => (Nil, Left(SyntaxError.EndingDot))
      case Token.Star(_) :: rest => (rest, Right(LensExpr.Each))
      case Token.Question(_) :: rest => (rest, Right(LensExpr.Opt))
      case Token.OpenBracket(_) :: Token.CloseBracket(_) :: rest => (rest, Left(SyntaxError.NoIndexSupplied))
      case Token.OpenBracket(_) :: tok :: Token.CloseBracket(_) :: rest => 
        (rest, parseIndex(tok).right.map(LensExpr.Indexed))
      case Nil => (Nil, Left(SyntaxError.EmptyError))
      case Token.OpenBracket(_) :: rest if !rest.exists { case Token.CloseBracket(_) => true; case _ => false } => (rest, Left(SyntaxError.UnclosedOpenBracket))
      case Token.OpenBracket(_) :: tok :: rest => (rest, Left(SyntaxError.InvalidIndexSupplied(tok)))
      case Token.CloseBracket(_) :: rest => (rest, Left(SyntaxError.UnexpectedCloseBracket))
      case Token.Name(n, _) :: rest => (rest, Left(SyntaxError.NameWithNoDot(n)))
      case Token.Hole :: rest => (rest, Left(SyntaxError.InterpOpticWithNoDot))
      case Token.Unrecognised(c, _) :: rest => (rest, Left(SyntaxError.UnrecognisedChar(c)))
    }
  }

  def parseLensRef(t: Token): Either[SyntaxError, LensRef] = {
    t match {
      case Token.Name(name, _) => Right(LensRef.Named(name)) 
      case Token.Hole => Right(LensRef.Interpolated)
      case Token.CloseBracket(_) => Left(SyntaxError.UnexpectedCloseBracket)
      case tok => Left(SyntaxError.InvalidAfterDot(tok))
    }
  }

  def parseIndex(t: Token): Either[SyntaxError, Index] = {
    t match {
      case Token.Name(intStr, _) => Try(intStr.toInt) match {
        case Success(i) => Right(Index.Literal(i))
        case Failure(_) => Left(SyntaxError.VerbatimIndexNotInt(intStr))
      }
      case Token.Hole => Right(Index.Interpolated)
      case Token.CloseBracket(_) => Left(SyntaxError.NoIndexSupplied)
      case x => Left(SyntaxError.InvalidIndexSupplied(x))
    }
  }
}
