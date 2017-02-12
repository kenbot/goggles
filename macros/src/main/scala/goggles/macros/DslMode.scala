package goggles.macros

sealed abstract class DslMode(val appliedToObject: Boolean)

object DslMode {
  case object Get extends DslMode(true)
  case object Set extends DslMode(true)
  case object Lens extends DslMode(false)
}