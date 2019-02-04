package goggles.macros.interpret

import goggles.macros.errors.GogglesError

private[goggles] case class MacroResult[+Type, +Tree](
  errorOrTree: Either[GogglesError[Type], Tree],
  infos: List[OpticInfo[Type]],
  lastSegmentOffset: Int)

