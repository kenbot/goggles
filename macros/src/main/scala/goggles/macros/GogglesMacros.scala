package goggles.macros

import goggles.macros.errors.ErrorMessages
import goggles.macros.interpret.dsl.{GetModeDslImpl, SetModeDslImpl, LensModeDslImpl}
import goggles.macros.interpret.{MacroResult, DslMode}

import scala.reflect.macros.whitebox


object GogglesMacros {

  def getImpl(ctx: whitebox.Context)(args: ctx.Expr[Any]*): ctx.Tree = {
    val interpreter = new GetModeDslImpl { override val c: ctx.type = ctx; }
    handleResult(ctx)(interpreter.get(args: _*), interpreter.mode)
  }

  def setImpl(ctx: whitebox.Context)(args: ctx.Expr[Any]*): ctx.Tree = {
    val interpreter = new SetModeDslImpl { override val c: ctx.type = ctx; }
    handleResult(ctx)(interpreter.set(args: _*), interpreter.mode)
  }

  def lensImpl(ctx: whitebox.Context)(args: ctx.Expr[Any]*): ctx.Tree = {
    val interpreter = new LensModeDslImpl { override val c: ctx.type = ctx; }
    handleResult(ctx)(interpreter.lens(args: _*), interpreter.mode)
  }

  private def handleResult(c: whitebox.Context)(result: MacroResult[c.Type, c.Tree], mode: DslMode): c.Tree = {

    result match {
      case MacroResult(Right(tree), _, _, _) => tree
      case MacroResult(Left(err), infos, remainingErrors, offset) =>
        val errMsg = ErrorMessages.message(err, mode, infos)
        val start = c.enclosingPosition
        c.abort(start.withPoint(start.point + offset), errMsg)
    }
  }
}
