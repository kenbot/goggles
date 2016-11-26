package goggles.macros

import scala.reflect.macros.whitebox
import monocle._


object MonocleInterpreter {

  import scalaz._, Scalaz._
  import AST._

  def interpretAppliedComposedLens(c: whitebox.Context)(acl: AppliedComposedLens): State[List[c.Expr[Any]], c.Tree] = {

    ???

  }

  def interpretComposedLens(c: whitebox.Context)(clens: ComposedLens): State[List[c.Expr[Any]], c.Tree] = {
    import c.universe._

    type ArgState[A] = State[List[c.Expr[Any]], A]

    val treeState: ArgState[List[c.Tree]] = 
    clens.list.traverse[ArgState, c.Tree](lexpr => interpretLensExpr(c)(lexpr))

    treeState.map { trees => 
      val head :: tail = trees 

      tail.foldLeft(q"$head"){(accum, tr) => 
        q"($accum).composeLens($tr)"
      }
    }
  }

  def interpretLensExpr(c: whitebox.Context)(lexpr: LensExpr): State[List[c.Expr[Any]], c.Tree] = State {
    (argsLeft: List[c.Expr[Any]]) => {

      import c.universe._

      def popArg(f: c.Expr[Any] => c.Tree): (List[c.Expr[Any]], c.Tree) = argsLeft match {
        case a :: as => (as, f(a))
        case Nil => (Nil, c.abort(c.enclosingPosition, "Internal error: ran out of args"))
      }

      lexpr match {
        case RefExpr(NamedLensRef(name)) => (argsLeft, q"${Ident(TermName(name))}") 
        case RefExpr(InterpLensRef) => popArg(a => q"($a)")        
        case EachExpr => (argsLeft, q"_root_.monocle.function.Each.each") 
        case OptExpr => (argsLeft, q"_root_.monocle.std.option.pSome") 
        case IndexedExpr(LiteralIndex(i)) => (argsLeft, q"_root_.monocle.function.Index.index(${Literal(Constant(i))})") 
        case IndexedExpr(InterpIndex) => popArg(a => q"_root_.monocle.function.Index.index($a)")        
      }
    }
  }
} 




object ContextUtils {

  def getFullContextString(implicit c: whitebox.Context): String = {
    import c.universe._
    getContextStringParts.foldLeft("") {
      case (full, str) => full + str
    }
  }

  def getContextStringParts(implicit c: whitebox.Context): List[String] = {
    import c.universe._

    c.prefix.tree match {
      case Apply(f, List(Apply(g, rawParts))) => rawParts.map {
        case Literal(Constant(str: String)) => str
      }
    }
  }

  def ident(name: String)(implicit c: whitebox.Context): c.Tree = {
    import c.universe._
    q"${Ident(TermName(name))}"
  }


  def getLensOrFail(errorMsg: String)(implicit c: whitebox.Context): c.Tree = {
    val tokens = getFullContextString.split("\\.").toList
    tokens match {
      case LensTree(tree) => tree
      case _ => c.abort(c.enclosingPosition, errorMsg)    
    }
  }

  object LensTree {
    def unapply(lensStrs: List[String])(implicit c: whitebox.Context): Option[c.Tree] = {
      import c.universe._
      lensStrs match {
        case Nil => None
        case firstLensStr :: otherLensStrs =>
          val firstLensIdent: c.Tree = ident(firstLensStr)
          val otherLensIdents = otherLensStrs.map(s => ident(s))

          val lensTree: c.Tree = otherLensIdents.foldLeft(firstLensIdent) {
            (tree, lens) => q"$tree.composeLens($lens)"
          }

          Some(lensTree)
      }
    }
  }
}


object Macros {
   
  def lensImpl(c: whitebox.Context)(args: c.Expr[Any]*): c.Tree = {
    import c.universe._
    import ContextUtils._

    val errorMsg = "Expecting one or more lenses separated " + 
                   "by operators \".\", \"*.\", \"?.\" or  " + 
                   "\"[n]\""

    val fragments = getContextStringParts(c) 
    val tokens = Lexer.lexFragments(fragments)
    val ast = Parser.parseComposedLens(tokens)
    val result = ast.map(t => MonocleInterpreter.interpretComposedLens(c)(t))

    result match {
      case Left(err) => c.abort(c.enclosingPosition, err.toString)
      case Right(resultState) => q"${resultState.run(args.toList)._2}" 
    }
  }

  def setImpl(c: whitebox.Context)(): c.Tree = {
    import c.universe._

    val errorMsg = "Invalid set\"...\" expression: " + 
                   "expecting at least a target object " + 
                   "followed by one or more composed " + 
                   "lenses: ie. " + 
                   "set\"myUser._company._headCount\" := 5"

    val (targetObj, lens) = getTargetAndLens(errorMsg)(c)

    q"new goggles.MonocleModifyOps($targetObj, $lens)"
  }

  def getImpl(c: whitebox.Context)(): c.Tree = {
    import c.universe._

    val errorMsg = "Invalid get\"...\" expression: " + 
                   "expecting at least a target object " + 
                   "followed by one or more composed " + 
                   "lenses: ie. " + 
                   "get\"myUser._company._headCount\""

    val (targetObj, lens) = getTargetAndLens(errorMsg)(c)
    q"($lens).get($targetObj)"
  }

  private def getTargetAndLens(errorMsg: String)(implicit c: whitebox.Context): (c.Tree, c.Tree) = {
    import c.universe._
    import ContextUtils._

    import scala.util.{Try, Success, Failure}

    val tokens = getFullContextString.split("\\.").toList

    tokens match {
      case targetObjStr :: LensTree(lensTree) =>
        (ident(targetObjStr), lensTree)

      case _ => 
        c.abort(c.enclosingPosition, errorMsg)    
    }

  }
}
