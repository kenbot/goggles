package goggles.macros.interpret

import goggles.macros.errors.GogglesError

private[goggles] case class MacroResult[+Type, +Tree](
  errorOrResult: Either[GogglesError[Type], Tree],
  infos: List[OpticInfo[Type]],
  lastSegmentOffset: Int)

