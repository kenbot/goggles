package goggles.macros.interpret.infrastructure

import goggles.macros.interpret.Parse
import scala.reflect.macros.whitebox

trait Contextual {
  val c: whitebox.Context
  
  type Interpret[A] = Parse[c.Type, c.Expr[Any], A]
}