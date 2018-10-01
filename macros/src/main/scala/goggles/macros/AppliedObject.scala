package goggles.macros

import monocle.PIso

object AppliedObject {
  def const[A,B](a: => A) = PIso[Unit, B, A ,B](_ => a)(identity)
}
