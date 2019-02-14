package goggles.macros

import goggles.macros.errors.ErrorMessages
import goggles.macros.interpret._

import scala.reflect.macros.whitebox


object GogglesMacros {

  def getImpl(ctx: whitebox.Context)(args: ctx.Expr[Any]*): ctx.Tree = {
    val interpreter = new GetModeImpl { override val c: ctx.type = ctx; }
    handleResult(ctx)(interpreter.get(args: _*), interpreter.mode)
  }

  def setImpl(ctx: whitebox.Context)(args: ctx.Expr[Any]*): ctx.Tree = {
    val interpreter = new SetModeImpl { override val c: ctx.type = ctx; }
    handleResult(ctx)(interpreter.set(args: _*), interpreter.mode)
  }

  def lensImpl(ctx: whitebox.Context)(args: ctx.Expr[Any]*): ctx.Tree = {
    val interpreter = new LensModeImpl { override val c: ctx.type = ctx; }
    handleResult(ctx)(interpreter.lens(args: _*), interpreter.mode)
  }

  private def handleResult(c: whitebox.Context)(
    result: MacroResult[c.Type, c.Tree], mode: DslMode): c.Tree = {

    result match {
      case MacroResult(Right(tree), _, _) => tree
      case MacroResult(Left(err), infos, offset) =>
        val errMsg = ErrorMessages.message(err, mode, infos)
        val start = c.enclosingPosition
        c.abort(start.withPoint(start.point + offset), errMsg)
    }
  }
}
