package goggles.macros

import scala.reflect.macros.whitebox
import monocle._

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
      case Apply(f, List(Apply(g, rawParts))) => rawParts
    }
    Nil
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
   
  def lensImpl(c: whitebox.Context)(): c.Tree = {
    import c.universe._
    import ContextUtils._

    val errorMsg = "Expecting one or more lenses separated " + 
                   "by operators \".\", \"*.\", \"?.\" or  " + 
                   "\"[n]\""

    val lensTree = getLensOrFail(errorMsg)(c)

    q"($lensTree)"
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
