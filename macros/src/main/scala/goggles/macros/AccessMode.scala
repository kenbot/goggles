package goggles.macros

sealed trait AccessMode

private[goggles] object AccessMode {
  case object Get extends AccessMode
  case object Set extends AccessMode
  case object GetAndSet extends AccessMode
}
