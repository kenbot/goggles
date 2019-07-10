package goggles.macros.interpret

import goggles.macros.errors.{ErrorAt, GogglesError}

case class MacroResult[+Type, +A](
    errorAtOrResult: Either[ErrorAt[Type], A], 
    infos: List[OpticInfo[Type]]) {
  
  def errorOrResult: Either[GogglesError[Type], A] = 
    errorAtOrResult.swap.map(_.error).swap
  
  def errorOffset: Option[Int] = errorAtOrResult match {
    case Left(err) => Some(err.offset)
    case Right(_) => None
  }
}