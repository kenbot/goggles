package goggles.macros.errors


case class ErrorAt[+Type](error: GogglesError[Type], offset: Int)