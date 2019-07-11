package goggles.macros.interpret.infrastructure

import goggles.macros.interpret._
import goggles.macros.errors.{GogglesError}

trait TypeCheckContext {
    this: Contextual => 
  
  def typeCheckOrElse(tree: c.Tree, orElse: => GogglesError[c.Type]): Interpret[c.Tree] = {
    val typed = c.typecheck(tree, silent = true)
    if (typed.isEmpty) Parse.raiseError(orElse)
    else Parse.pure(typed)
  }


  def patternMatchOrElse[A, R](a: A, orElse: => GogglesError[c.Type])(pf: PartialFunction[A,R]): Interpret[R] =
    if (pf.isDefinedAt(a)) Parse.pure(pf(a))
    else Parse.raiseError(orElse)
}