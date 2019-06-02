package goggles.macros.interpret

import goggles.macros.errors.{GogglesError, NotEnoughArguments, EmptyError}
import goggles.macros.parse.AST.{ComposedLensExpr, LensExpr}

private[goggles] trait Parse[Type, Arg, A] {
  self =>

  def apply(state: MacroState[Type,Arg]): (Either[GogglesError[Type],A], MacroState[Type,Arg])

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

  final def eval(args: List[Arg], mode: DslMode): MacroResult[Type, A] = {
    val (errorOrA, macroState) = apply(MacroState(args, None, Nil, Nil, 0))
    MacroResult(errorOrA, macroState.infos, macroState.remainingLensExprs, SourcePosition.getErrorOffset(mode, macroState))
  }
}

private[goggles] object Parse {
  def pure[Type, Arg, A](a: => A): Parse[Type,Arg,A] = 
    state => (Right(a), state)

  def fromOption[Type, Arg, A](opt: Option[A], orElse: => GogglesError[Type]): Parse[Type, Arg, A] = opt match {
    case Some(a) => pure(a)
    case None => raiseError(orElse)
  }

  def loadLensExprs[Type, Arg](either: Either[GogglesError[Type], ComposedLensExpr]): Parse[Type,Arg,Unit] = {
    state0 => 
      val state = either match {
        case Left(err) => state0
        case Right(composedLensExpr) => state0.copy(remainingLensExprs = composedLensExpr.exprs)
      }
      (either.map(_ => ()), state)
  }

  def getMacroState[Type, Arg]: Parse[Type, Arg, MacroState[Type, Arg]] = 
    state => (Right(state), state)

  def fromEither[Type, Arg, A](either: Either[GogglesError[Type], A]): Parse[Type,Arg,A] = 
    state => (either, state)

  def raiseError[Type, Arg, A](e: GogglesError[Type]): Parse[Type, Arg, A] = 
    state => (Left(e), state)

  def getLastOpticInfo[Type,Arg]: Parse[Type, Arg, Option[OpticInfo[Type]]] = 
      state => (Right(state.lastOpticInfo), state)

  def getLastOpticInfoOrElse[Type,Arg](orElse: => GogglesError[Type]): Parse[Type, Arg, OpticInfo[Type]] = {
    state => state.lastOpticInfo match {
      case Some(info) => (Right(info), state)
      case None => (Left(orElse), state)
    }
  }

  def storeOpticInfo[Type,Arg](info: OpticInfo[Type]): Parse[Type, Arg, Unit] = 
    state => (Right(()), state.addOpticInfo(info))

  def popArg[Type, Arg]: Parse[Type, Arg, Arg] = {
    state0 => state0.popArg match {
      case Some((arg, state)) => (Right(arg), state)
      case None => (Left(NotEnoughArguments), state0)
    }
  }

  def popLensExprMaybe[Type, Arg]: Parse[Type, Arg, Option[LensExpr]] = {
    state0 => state0.popLensExpr match {
      case Some((lensExpr, state)) => (Right(Some(lensExpr)), state)
      case None => (Right(None), state0)
    }
  }

  def popLensExpr[Type, Arg]: Parse[Type, Arg, LensExpr] = {
    state0 => state0.popLensExpr match {
      case Some((lensExpr, state)) => (Right(lensExpr), state)
      case None => (Left(EmptyError), state0)
    }
  }

  def remainingLensExprs[Type, Arg]: Parse[Type, Arg, List[LensExpr]] = {
    state => (Right(state.remainingLensExprs), state)
  }

  def addToOffset[Type, Arg](delta: Int): Parse[Type, Arg, Unit] = 
    state => (Right(()), state.addToOffset(delta))
}