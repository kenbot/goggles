package goggles.macros.interpret

import goggles.macros.errors.{GogglesError, NotEnoughArguments}

import scalaz._

case class ParseState[T,Arg](args: List[Arg], infos: List[OpticInfo[T]])

trait Parse[T, Arg, A] {
  self =>

  def apply(state: ParseState[T,Arg]): (Either[GogglesError[T],A], ParseState[T,Arg])

  final def map[B](f: A => B) = new Parse[T,Arg,B] {
    def apply(state0: ParseState[T,Arg]): (Either[GogglesError[T],B], ParseState[T,Arg]) = {
      val (errorOrA, state) = self(state0)
      (errorOrA.right.map(f), state)
    }
  }

  final def flatMap[B](f: A => Parse[T,Arg,B]) = new Parse[T,Arg,B] {
    def apply(state0: ParseState[T,Arg]): (Either[GogglesError[T],B], ParseState[T,Arg]) = {
      self(state0) match {
        case (Right(a), state) => f(a)(state)
        case (Left(err), state) => (Left(err), state)
      }
    }
  }

  final def eval(args: List[Arg]): (Either[GogglesError[T],A], List[OpticInfo[T]]) = {
    val (errorOrA, ParseState(_, infos)) = apply(ParseState(args, Nil))
    (errorOrA, infos.reverse)
  }
}

object Parse {
  def pure[T,Arg,A](a: => A) = new Parse[T,Arg,A] {
    def apply(state: ParseState[T,Arg]): (Either[GogglesError[T],A], ParseState[T,Arg]) = {
      (Right(a), state)
    }
  }

  def fromOption[T, Arg, A](opt: Option[A], orElse: => GogglesError[T]): Parse[T, Arg, A] = opt match {
    case Some(a) => pure(a)
    case None => raiseError(orElse)
  }

  def fromEither[T, Arg, A](either: Either[GogglesError[T],A]) = new Parse[T,Arg,A] {
    def apply(state: ParseState[T,Arg]): (Either[GogglesError[T],A], ParseState[T,Arg]) = {
      (either, state)
    }
  }

  def raiseError[T, Arg, A](e: GogglesError[T]) = new Parse[T, Arg, A] {
    def apply(state: ParseState[T,Arg]): (Either[GogglesError[T],A], ParseState[T,Arg]) = {
      (Left(e), state)
    }
  }

  def getLastOpticInfo[T,Arg] = new Parse[T, Arg, Option[OpticInfo[T]]] {
    def apply(state: ParseState[T,Arg]): (Either[GogglesError[T],Option[OpticInfo[T]]], ParseState[T,Arg]) = {
      (Right(state.infos.headOption), state)
    }
  }

  def getLastOpticInfoOrElse[T,Arg](orElse: => GogglesError[T]) = new Parse[T, Arg, OpticInfo[T]] {
    def apply(state: ParseState[T,Arg]): (Either[GogglesError[T],OpticInfo[T]], ParseState[T,Arg]) = state.infos match {
      case info :: _ => (Right(info), state)
      case Nil => (Left(orElse), state)
    }
  }

  def storeOpticInfo[T,Arg](info: OpticInfo[T]) = new Parse[T, Arg, Unit]  {
    def apply(state: ParseState[T,Arg]): (Either[GogglesError[T],Unit], ParseState[T,Arg]) = {
      (Right(()), ParseState(state.args, info :: state.infos))
    }
  }

  def popArg[T, Arg] = new Parse[T, Arg, Arg] {
    def apply(state: ParseState[T,Arg]): (Either[GogglesError[T],Arg], ParseState[T,Arg]) = state.args match {
      case arg :: rest => (Right(arg), ParseState(rest, state.infos))
      case Nil => (Left(NotEnoughArguments), state)
    }
  }

  implicit def monad[T,Arg] = new Monad[({type f[a]=Parse[T,Arg,a]})#f] {
    override def bind[A, B](fa: Parse[T,Arg,A])(f: A => Parse[T,Arg,B]): Parse[T,Arg,B] = fa.flatMap(f)
    override def point[A](a: => A): Parse[T,Arg,A] = pure(a)
  }
}