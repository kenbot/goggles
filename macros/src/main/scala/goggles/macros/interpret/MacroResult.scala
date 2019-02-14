package goggles.macros.interpret

import goggles.macros.errors.GogglesError

private[goggles] case class MacroResult[+Type, +A](
  errorOrResult: Either[GogglesError[Type], A],
  infos: List[OpticInfo[Type]],
  lastSegmentOffset: Int)

