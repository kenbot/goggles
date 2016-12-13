package goggles.macros

import scalaz._, Scalaz._

trait Parse[S, Arg, A] {
  self =>

  case class ParseState(state: S, args: List[Arg])

  def apply(state: S, args: List[Arg]): (Either[ParseError,A], S, List[Arg])

  final def map[B](f: A => B): Parse[S,Arg,B] = { 
    case (s,args) =>
      val r = self(s,args)
      (r._1.map(f), r._2, r._3)
  }

  final def flatMap[B](f: A => Parse[S,Arg,B]): Parse[S,Arg,B] = { case (s0,args0) =>
    self(s0,args0) match {
      case (Right(a), s, args) => f(a)(s,args)
      case (Left(err), s, args) => (Left(err), s, args)
    }
  }

  final def eval(state: S, args: List[Arg]): Either[ParseError,A] =  
    apply(state, args)._1
}

object Parse {
  def result[S,Arg,A](a: => A): Parse[S,Arg,A] = {
    case (s,args) => (Right(a), s, args)
  }

  def raiseError[S,Arg,A](e: ParseError): Parse[S,Arg,A] = {
    case (s,args) => (Left(e), s, args)
  }

  def getState[S,Arg]: Parse[S,Arg,S] = {
    case (s,args) => (Right(s), s, args)
  }

  def setState[S,Arg](newS: S): Parse[S,Arg,Unit] = {
    case (_,args) => (Right(()), newS, args)
  }

  def popArg[S,Arg]: Parse[S,Arg,Arg] = {
    case (s, arg :: rest) => (Right(arg), s, rest)
    case (s, Nil) => (Left(argumentsExhausted), s, Nil)
  }

  def useArg[S,Arg,A](f: Arg => A): Parse[S,Arg,A] =
    popArg.map(f)

  def getArguments[S,Arg]: Parse[S,Arg,List[Arg]] = {
    case (s,args) => (Right(args), s, args)
  }

  private def argumentsExhausted =
    InternalError("Internal error: ran out of arguments.")

  implicit def monad[S,Arg] = new Monad[({type f[a]=Parse[S,Arg,a]})#f] {
    override def bind[A, B](fa: Parse[S,Arg,A])(f: A => Parse[S,Arg,B]): Parse[S,Arg,B] = fa.flatMap(f)
    override def point[A](a: => A): Parse[S,Arg,A] = result(a)
  }
}
