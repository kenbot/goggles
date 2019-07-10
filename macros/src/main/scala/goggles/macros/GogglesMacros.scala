package goggles.macros

import goggles.macros.interpret.dsl.{GetModeDslImpl, SetModeDslImpl, LensModeDslImpl}
import scala.reflect.macros.whitebox


object GogglesMacros {

  def getImpl(ctx: whitebox.Context)(args: ctx.Expr[Any]*): ctx.Tree = {
    val interpreter = new GetModeDslImpl { override val c: ctx.type = ctx; }
    interpreter.handleMacroResultOf(_.get(args: _*))
  }

  def setImpl(ctx: whitebox.Context)(args: ctx.Expr[Any]*): ctx.Tree = {
    val interpreter = new SetModeDslImpl { override val c: ctx.type = ctx; }
    interpreter.handleMacroResultOf(_.set(args: _*))
  }

  def lensImpl(ctx: whitebox.Context)(args: ctx.Expr[Any]*): ctx.Tree = {
    val interpreter = new LensModeDslImpl { override val c: ctx.type = ctx; }
    interpreter.handleMacroResultOf(_.lens(args: _*))
  }
}
