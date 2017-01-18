package goggles.macros

sealed trait DslMode

object DslMode {
  case object Get extends DslMode
  case object Set extends DslMode
  case object Lens extends DslMode
}