package goggles.macros.interpret

import goggles.macros.errors.{GogglesError, InternalError, ErrorAt}

private[goggles] trait Parse[Type, Arg, A] {
  self =>

  def apply(state: MacroState[Type,Arg]): (Either[ErrorAt[Type],A], MacroState[Type,Arg])

  final def map[B](f: A => B): Parse[Type,Arg,B] = {
    state0 => {
      val (errorOrA, state) = self(state0)
      (errorOrA.right.map(f), state)
    }
  }

  final def flatMap[B](f: A => Parse[Type,Arg,B]): Parse[Type,Arg,B] = {
    state0 => self(state0) match {
      case (Right(a), state) => f(a)(state)
      case (Left(err), state) => (Left(err), state)
    }
  }

  final def eval(args: List[Arg]): MacroResult[Type, A] = {
    val (errorOrA, macroState) = apply(MacroState.blank(args))
    MacroResult(errorOrA, macroState.infos)
  }
}

private[goggles] object Parse {
  def pure[Type, Arg, A](a: => A): Parse[Type,Arg,A] = 
    state => (Right(a), state)

  def fromOption[Type, Arg, A](opt: Option[A], orElse: => GogglesError[Type]): Parse[Type, Arg, A] = opt match {
    case Some(a) => pure(a)
    case None => raiseError(orElse)
  }

  def getMacroState[Type, Arg]: Parse[Type, Arg, MacroState[Type, Arg]] = 
    state => (Right(state), state)

  def getCurrentExprOffset[Type, Arg]: Parse[Type, Arg, Int] = 
    getMacroState.map(_.currentExprOffset)

  def setCurrentExprOffset[Type, Arg](offset: Int): Parse[Type, Arg, Unit] = 
    state => (Right(()), state.copy(currentExprOffset = offset))

  def fromEither[Type, Arg, A](either: Either[ErrorAt[Type], A]): Parse[Type,Arg,A] = 
    state => (either, state)

  def raiseError[Type, Arg, A](e: GogglesError[Type]): Parse[Type, Arg, A] = 
    state => (Left(e.at(state.currentExprOffset)), state)

  def getLastOpticInfo[Type,Arg]: Parse[Type, Arg, Option[OpticInfo[Type]]] = 
      state => (Right(state.lastOpticInfo), state)

  def getLastOpticInfoOrElse[Type,Arg](orElse: => GogglesError[Type]): Parse[Type, Arg, OpticInfo[Type]] = {
    state => state.lastOpticInfo match {
      case Some(info) => (Right(info), state)
      case None => (Left(orElse.at(state.currentExprOffset)), state)
    }
  }

  def storeOpticInfo[Type,Arg](info: OpticInfo[Type]): Parse[Type, Arg, Unit] = 
    state => (Right(()), state.addOpticInfo(info))

  def popArg[Type, Arg]: Parse[Type, Arg, Arg] = {
    state0 => state0.popArg match {
      case Some((arg, state)) => (Right(arg), state)
      case None => (Left(InternalError.NotEnoughArguments.at(state0.currentExprOffset)), state0)
    }
  }
}