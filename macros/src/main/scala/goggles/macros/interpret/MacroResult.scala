package goggles.macros.interpret

import goggles.macros.errors.GogglesError

private[goggles] case class MacroResult[Type, Tree](
  errorOrTree: Either[GogglesError[Type], Tree],
  infos: List[OpticInfo[Type]],
  lastSegmentOffset: Int)

private[goggles] object MacroResult {
  def tupled[Type,Tree](tuple: (Either[GogglesError[Type], Tree],
                                List[OpticInfo[Type]])) = {
    MacroResult(tuple._1, tuple._2, 0)
  }
}

