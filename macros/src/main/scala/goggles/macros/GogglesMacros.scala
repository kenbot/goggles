package goggles.macros

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
    result: (Either[GogglesError[c.Type], c.Tree], List[ParseInfo[c.Type]]), mode: DslMode): c.Tree = {

    result match {
      case (Right(tree), _) => tree
      case (Left(err), infos) =>
        val errMsg = ErrorMessages.message(err, mode, infos)
        c.abort(c.enclosingPosition, errMsg)
    }
  }
}