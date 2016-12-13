package goggles.macros

import monocle.PSetter

class MonocleModifyOps[T,A,B](setter: PSetter[Unit,T,A,B]) extends ModifyOps[Unit,T,A,B] {
  override def ~=(f: A => B): T = setter.modify(f)(())
}
