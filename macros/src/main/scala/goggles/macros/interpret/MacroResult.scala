package goggles.macros.interpret

import goggles.macros.errors.GogglesError
import goggles.macros.parse.LensExpr

case class MacroResult[+Type, +A](
  errorOrResult: Either[GogglesError[Type], A],
  infos: List[OpticInfo[Type]],
  remainingExprs: List[LensExpr],
  lastSegmentOffset: Int)

  

