package goggles.macros

import goggles.macros.OpticType.{LensType, SetterType, GetterType}

sealed abstract class AccessMode(val opticType: OpticType)

private[goggles] object AccessMode {
  case object Get extends AccessMode(GetterType)
  case object Set extends AccessMode(SetterType)
  case object GetAndSet extends AccessMode(LensType)
}
