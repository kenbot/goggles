package goggles.macros.interpret.infrastructure

import goggles.macros.interpret._
import goggles.macros.errors.{ErrorMessages}

trait HandleResultsContext {
    this: Contextual 
    with DslModeContext 
    with StringContextsContext => 

  def handleMacroResultOf(f: this.type => MacroResult[c.Type, c.Tree]): c.Tree = 
    handleMacroResult(f(this))

  private def handleMacroResult(result: MacroResult[c.Type, c.Tree]): c.Tree = {
    result match {
      case MacroResult(Right(tree), _) => tree
      case MacroResult(Left(errorAt), infos) =>
        val errMsg = ErrorMessages.message(errorAt.error, mode, infos)
        c.abort(c.enclosingPosition.withPoint(getAbsoluteOffset(errorAt.offset)), errMsg)
    }
  }
}