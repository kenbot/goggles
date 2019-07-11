package goggles.macros

import goggles.macros.errors.{GogglesError, ErrorAt}

// Attaches an integer offset to somethign
case class At[A](value: A, offset: Int) {
  def toError[Type](e: GogglesError[Type]): ErrorAt[Type] = 
    e.at(offset)
}