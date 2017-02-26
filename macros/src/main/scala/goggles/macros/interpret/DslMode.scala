package goggles.macros.interpret

import DslMode._
import OpticType.{LensType, SetterType, GetterType}


private[goggles] sealed trait DslMode {
  def isReadOnly = this == Get

  def opticType = this match {
    case Get => GetterType
    case Set => SetterType
    case Lens => LensType
  }

  def appliedToObject = this match {
    case Get | Set => true
    case Lens => false
  }
}

private[goggles] object DslMode {
  case object Get extends DslMode
  case object Set extends DslMode
  case object Lens extends DslMode
}