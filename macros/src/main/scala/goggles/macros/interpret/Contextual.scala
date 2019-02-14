package goggles.macros.interpret

import scala.reflect.macros.whitebox

trait Contextual {
  val c: whitebox.Context
  
  type Interpret[A] = Parse[c.Type, c.Expr[Any], A]
}