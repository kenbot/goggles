package goggles.macros

import goggles.macros.errors.ErrorMessages
import goggles.macros.interpret.{MacroResult, MacroInterpreter, DslMode}

import scala.reflect.macros.whitebox


object GogglesMacros {

  def getImpl(c: whitebox.Context)(args: c.Expr[Any]*): c.Tree = {
    handleResult(c)(MacroInterpreter.getImpl(c)(args: _*), DslMode.Get)
  }

  def setImpl(c: whitebox.Context)(args: c.Expr[Any]*): c.Tree = {
    handleResult(c)(MacroInterpreter.setImpl(c)(args: _*), DslMode.Set)
  }

  def lensImpl(c: whitebox.Context)(args: c.Expr[Any]*): c.Tree = {
    handleResult(c)(MacroInterpreter.lensImpl(c)(args: _*), DslMode.Lens)
  }

  private def handleResult(c: whitebox.Context)(
    result: MacroResult[c.Type, c.Tree], mode: DslMode): c.Tree = {

    result match {
      case MacroResult(Right(tree), _) => tree
      case MacroResult(Left(err), infos) =>
        val errMsg = ErrorMessages.message(err, mode, infos)
        c.abort(c.enclosingPosition, errMsg)
    }
  }
}