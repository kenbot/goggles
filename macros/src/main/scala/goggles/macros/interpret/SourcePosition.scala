package goggles.macros.interpret

import goggles.macros.parse.AST._

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
      case Some(RefExpr(NamedLensRef(name))) => 1   // .
      case Some(RefExpr(InterpLensRef)) => 2        // .$
      case Some(IndexedExpr(LiteralIndex(_))) => 1  // [
      case Some(IndexedExpr(InterpIndex)) => 2      // [$
      case _ => 0
    }

    prefix + infos.foldLeft(0)(_ + _.label.size) + skipSyntax
  }

  def getSegmentWidth(expr: LensExpr): Int = {
    val dot = 1
    val brackets = 2
    expr match {
      case RefExpr(NamedLensRef(name)) => dot + name.length
      case RefExpr(InterpLensRef) => dot + 0 
      case EachExpr => 1
      case OptExpr => 1
      case IndexedExpr(LiteralIndex(i)) => brackets + i.toString.length
      case IndexedExpr(InterpIndex) => brackets + 0 
    }
  }

  def calcArgWidth(c: whitebox.Context)(arg: c.Tree): Int = {
    val w = arg.pos.end - arg.pos.start
    w
    // Determine whether there are curlies that need to be accounted for
  }
} 
