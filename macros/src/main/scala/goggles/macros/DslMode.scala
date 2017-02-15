package goggles.macros

import DslMode._
import goggles.macros.OpticType.{LensType, SetterType, GetterType}

sealed trait DslMode {
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

object DslMode {
  case object Get extends DslMode
  case object Set extends DslMode
  case object Lens extends DslMode
}