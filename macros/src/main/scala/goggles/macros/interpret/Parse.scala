package goggles.macros.interpret

import goggles.macros.errors.{GogglesError, NotEnoughArguments}

import scalaz._

private[goggles] case class ParseState[Type,Arg](args: List[Arg], infos: List[OpticInfo[Type]], offset: Int)

private[goggles] trait Parse[Type, Arg, A] {
  self =>

  def apply(state: ParseState[Type,Arg]): (Either[GogglesError[Type],A], ParseState[Type,Arg])

  final def map[B](f: A => B) = new Parse[Type,Arg,B] {
    def apply(state0: ParseState[Type,Arg]): (Either[GogglesError[Type],B], ParseState[Type,Arg]) = {
      val (errorOrA, state) = self(state0)
      (errorOrA.right.map(f), state)
    }
  }

  final def flatMap[B](f: A => Parse[Type,Arg,B]) = new Parse[Type,Arg,B] {
    def apply(state0: ParseState[Type,Arg]): (Either[GogglesError[Type],B], ParseState[Type,Arg]) = {
      self(state0) match {
        case (Right(a), state) => f(a)(state)
        case (Left(err), state) => (Left(err), state)
      }
    }
  }

  final def eval(args: List[Arg]): (Either[GogglesError[Type],A], List[OpticInfo[Type]]) = {
    val (errorOrA, ParseState(_, infos, _)) = apply(ParseState(args, Nil, 0))
    (errorOrA, infos.reverse)
  }
}

private[goggles] object Parse {
  def pure[Type,Arg,A](a: => A) = new Parse[Type,Arg,A] {
    def apply(state: ParseState[Type,Arg]): (Either[GogglesError[Type],A], ParseState[Type,Arg]) = {
      (Right(a), state)
    }
  }

  def fromOption[Type, Arg, A](opt: Option[A], orElse: => GogglesError[Type]): Parse[Type, Arg, A] = opt match {
    case Some(a) => pure(a)
    case None => raiseError(orElse)
  }

  def fromEither[Type, Arg, A](either: Either[GogglesError[Type],A]) = new Parse[Type,Arg,A] {
    def apply(state: ParseState[Type,Arg]): (Either[GogglesError[Type],A], ParseState[Type,Arg]) = {
      (either, state)
    }
  }

  def raiseError[Type, Arg, A](e: GogglesError[Type]) = new Parse[Type, Arg, A] {
    def apply(state: ParseState[Type,Arg]): (Either[GogglesError[Type],A], ParseState[Type,Arg]) = {
      (Left(e), state)
    }
  }

  def getLastOpticInfo[Type,Arg] = new Parse[Type, Arg, Option[OpticInfo[Type]]] {
    def apply(state: ParseState[Type,Arg]): (Either[GogglesError[Type],Option[OpticInfo[Type]]], ParseState[Type,Arg]) = {
      (Right(state.infos.headOption), state)
    }
  }

  def getLastOpticInfoOrElse[Type,Arg](orElse: => GogglesError[Type]) = new Parse[Type, Arg, OpticInfo[Type]] {
    def apply(state: ParseState[Type,Arg]): (Either[GogglesError[Type],OpticInfo[Type]], ParseState[Type,Arg]) = state.infos match {
      case info :: _ => (Right(info), state)
      case Nil => (Left(orElse), state)
    }
  }

  def storeOpticInfo[Type,Arg](info: OpticInfo[Type]) = new Parse[Type, Arg, Unit]  {
    def apply(state: ParseState[Type,Arg]): (Either[GogglesError[Type],Unit], ParseState[Type,Arg]) = {
      (Right(()), state.copy(infos = info :: state.infos))
    }
  }

  def popArg[Type, Arg] = new Parse[Type, Arg, Arg] {
    def apply(state: ParseState[Type,Arg]): (Either[GogglesError[Type],Arg], ParseState[Type,Arg]) = state.args match {
      case arg :: rest => (Right(arg), state.copy(args = rest))
      case Nil => (Left(NotEnoughArguments), state)
    }
  }

  def addToOffset[Type, Arg](delta: Int) = new Parse[Type, Arg, Unit] {
    def apply(state: ParseState[Type,Arg]): (Either[GogglesError[Type],Unit], ParseState[Type,Arg]) = {
      (Right(()), state.copy(offset = state.offset + delta))
    }
  }

  implicit def monad[Type,Arg] = new Monad[({type f[a]=Parse[Type,Arg,a]})#f] {
    override def bind[A, B](fa: Parse[Type,Arg,A])(f: A => Parse[Type,Arg,B]): Parse[Type,Arg,B] = fa.flatMap(f)
    override def point[A](a: => A): Parse[Type,Arg,A] = pure(a)
  }
}