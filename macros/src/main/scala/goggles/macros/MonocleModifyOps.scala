package goggles.macros

import monocle.PSetter

class MonocleModifyOps[S,T,A,B](targetObj: S, setter: PSetter[S,T,A,B]) extends ModifyOps[S,T,A,B] {
  override def ~=(f: A => B): T = setter.modify(f)(targetObj)
}
