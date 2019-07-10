package goggles.macros.parse

import goggles.macros.At
import goggles.macros.lex.Token
import goggles.macros.errors.{ErrorAt, SyntaxError}

import scala.util.{Try, Success, Failure}

private[goggles] object Parser {

  def parseAppliedLens(tokens: List[At[Token]]): Either[ErrorAt[Nothing], AST] = {
    tokens match {
      case Nil => Left(SyntaxError.EmptyError.at(0))
      case At(Token.Hole, _) :: rest => parseAST(rest)
      case At(Token.Unrecognised(c), offset) :: _ => Left(SyntaxError.UnrecognisedChar(c).at(offset))
      case At(token, offset) :: _ => Left(SyntaxError.NonInterpolatedStart(token).at(offset))
    }
  }

  def parseUnappliedLens(tokens: List[At[Token]]): Either[ErrorAt[Nothing], AST] = {
    tokens match {
      case Nil => Left(SyntaxError.EmptyError.at(0))
      case At(Token.Hole, offset) :: rest => parseAST(Token.Dot.at(offset-1) :: Token.Hole.at(offset) :: rest)
      case At(Token.Unrecognised(c), offset) :: _ => Left(SyntaxError.UnrecognisedChar(c).at(offset))
      case At(token, offset) :: _ => Left(SyntaxError.NonInterpolatedStart(token).at(offset))
    }
  }

  private def parseAST(tokens: List[At[Token]]): Either[ErrorAt[Nothing], AST] = {

    def loop(remaining: List[At[Token]], exprs: List[At[LensExpr]]): Either[ErrorAt[Nothing], AST] = {
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

  def parseLensExpr(tokens: List[At[Token]]): (List[At[Token]], Either[ErrorAt[Nothing], At[LensExpr]]) = {
    tokens match {
      case At(Token.Dot, _) :: tokenAt :: rest => (rest, parseLensRef(tokenAt).map(ref => LensExpr.Ref(ref).at(tokenAt.offset)))
      case At(Token.Dot, offset) :: Nil  => (Nil, Left(SyntaxError.EndingDot.at(offset)))
      case At(Token.Star, offset) :: rest => (rest, Right(LensExpr.Each.at(offset)))
      case At(Token.Question, offset) :: rest => (rest, Right(LensExpr.Opt.at(offset)))
      case At(Token.OpenBracket,_) :: At(Token.CloseBracket, offset) :: rest => (rest, Left(SyntaxError.NoIndexSupplied.at(offset)))
      case At(Token.OpenBracket,_) :: tokenAt :: At(Token.CloseBracket,_) :: rest => (rest, parseIndex(tokenAt).map(ix => LensExpr.Indexed(ix).at(tokenAt.offset)))
      case Nil => (Nil, Left(SyntaxError.EmptyError.at(0)))
      case At(Token.OpenBracket, offset) :: rest 
        if !rest.exists { 
          case At(Token.CloseBracket,_) => true; 
          case _ => false 
        } => (rest, Left(SyntaxError.UnclosedOpenBracket.at(offset)))
      case At(Token.OpenBracket,_) :: At(token, offset) :: rest => (rest, Left(SyntaxError.InvalidIndexSupplied(token).at(offset)))
      case At(Token.CloseBracket, offset) :: rest => (rest, Left(SyntaxError.UnexpectedCloseBracket.at(offset)))
      case At(Token.Name(n), offset) :: rest => (rest, Left(SyntaxError.NameWithNoDot(n).at(offset)))
      case At(Token.Hole, offset) :: rest => (rest, Left(SyntaxError.InterpOpticWithNoDot.at(offset)))
      case At(Token.Unrecognised(c), offset) :: rest => (rest, Left(SyntaxError.UnrecognisedChar(c).at(offset)))
    }
  }

  def parseLensRef(tokenAt: At[Token]): Either[ErrorAt[Nothing], LensRef] = {
    tokenAt match {
      case At(Token.Name(name), _) => Right(LensRef.Named(name)) 
      case At(Token.Hole, _) => Right(LensRef.Interpolated)
      case At(Token.CloseBracket, offset) => Left(SyntaxError.UnexpectedCloseBracket.at(offset))
      case At(token, offset) => Left(SyntaxError.InvalidAfterDot(token).at(offset))
    }
  }

  def parseIndex(tokenAt: At[Token]): Either[ErrorAt[Nothing], Index] = {
    tokenAt match {
      case At(Token.Name(intStr), offset) => Try(intStr.toInt) match {
        case Success(i) => Right(Index.Literal(i))
        case Failure(_) => Left(SyntaxError.VerbatimIndexNotInt(intStr).at(offset))
      }
      case At(Token.Hole, _) => Right(Index.Interpolated)
      case At(Token.CloseBracket, offset) => Left(SyntaxError.NoIndexSupplied.at(offset))
      case At(x, offset) => Left(SyntaxError.InvalidIndexSupplied(x).at(offset))
    }
  }
}
