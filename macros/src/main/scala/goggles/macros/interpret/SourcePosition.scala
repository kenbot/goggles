package goggles.macros.interpret

import goggles.macros.parse._

import scala.reflect.macros.whitebox

object SourcePosition {
  def getErrorOffset(mode: DslMode, macroState: MacroState[_,_]): Int = {

    import DslMode._
    val verb = mode match {
      case Get | Set => 3
      case Lens => 4
    }

    val infos = macroState.infos
    val openQuote = 1
    val prefix = verb + openQuote

    val skipSyntax = macroState.currentLensExpr match {
      case Some(LensExpr.Ref(LensRef.Named(name))) => 1   // .
      case Some(LensExpr.Ref(LensRef.Interpolated)) => 2        // .$
      case Some(LensExpr.Indexed(Index.Literal(_))) => 1  // [
      case Some(LensExpr.Indexed(Index.Interpolated)) => 2      // [$
      case _ => 0
    }

    prefix + infos.foldLeft(0)(_ + _.label.size) + skipSyntax
  }

  def getSegmentWidth(expr: LensExpr): Int = {
    val dot = 1
    val brackets = 2
    expr match {
      case LensExpr.Ref(LensRef.Named(name)) => dot + name.length
      case LensExpr.Ref(LensRef.Interpolated) => dot + 0 
      case LensExpr.Each => 1
      case LensExpr.Opt => 1
      case LensExpr.Indexed(Index.Literal(i)) => brackets + i.toString.length
      case LensExpr.Indexed(Index.Interpolated) => brackets + 0 
    }
  }

  def calcArgWidth(c: whitebox.Context)(arg: c.Tree): Int = {
    val w = arg.pos.end - arg.pos.start
    w
    // Determine whether there are curlies that need to be accounted for
  }
} 
