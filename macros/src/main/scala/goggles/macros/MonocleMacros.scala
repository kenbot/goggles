package goggles.macros

import scala.reflect.macros.whitebox
import monocle._


object MonocleMacros {

  import scalaz._, Scalaz._
  import AST._

  def lensImpl(c: whitebox.Context)(args: c.Expr[Any]*): c.Tree = {
    import c.universe._

    val errorMsg = "Expecting one or more lenses separated " + 
                   "by operators \".\", \"*.\", \"?.\" or  " + 
                   "\"[n]\""

    val errorOrAst = Parser.parseComposedLens(Lexer(getContextStringParts(c)))

    val resultState = errorOrAst.map(ast => 
      interpretComposedLens(c)(ast).eval(args.toList))

    resultState match {
      case Left(err) => c.abort(c.enclosingPosition, err.toString)
      case Right(tree) => q"$tree" 
    }
  }

  def setImpl(c: whitebox.Context)(args: c.Expr[Any]*): c.Tree = {
    import c.universe._
    import AST._

    val errorMsg = "Invalid set\"...\" expression: " + 
                   "expecting at least a target object " + 
                   "followed by one or more composed " + 
                   "lenses: ie. " + 
                   "set\"myUser._company._headCount\" := 5"

    val errorOrAst = Parser.parseAppliedComposedLens(Lexer(getContextStringParts(c)))

    val resultState = errorOrAst.map { 
      case AppliedComposedLens(target, lens) => 
        for {
          lensTree <- interpretComposedLens(c)(lens)
          targetTree <- interpretTarget(c)(target)
        } yield q"(new _root_.goggles.MonocleModifyOps($targetTree, ($lensTree).asSetter))"
    }

    resultState match {
      case Left(err) => c.abort(c.enclosingPosition, err.toString)
      case Right(tree) => q"${tree.eval(args.toList)}"
    }
  }

  def getImpl(c: whitebox.Context)(args: c.Expr[Any]*): c.Tree = {
    import c.universe._
    import AST._

    val errorMsg = "Invalid get\"...\" expression: " + 
                   "expecting at least a target object " + 
                   "followed by one or more composed " + 
                   "lenses: ie. " + 
                   "get\"myUser._company._headCount\""

    val errorOrAst = Parser.parseAppliedComposedLens(Lexer(getContextStringParts(c)))

    val resultState = errorOrAst.map { 
      case AppliedComposedLens(target, lens) => 
        for {
          lensTree <- interpretComposedLens(c)(lens)
          targetTree <- interpretTarget(c)(target)
        } yield {
          typeCheckUntilSomethingWorks(c)(
            q"($lensTree).get($targetTree)",
            q"($lensTree).getAll($targetTree)",
            q"($lensTree).getOption($targetTree)")
        }
    }

    resultState match {
      case Left(err) => c.abort(c.enclosingPosition, err.toString)
      case Right(tree) => q"${tree.eval(args.toList)}"
    }
  }

  private def interpretTarget(c: whitebox.Context)(target: Target): State[List[c.Expr[Any]], c.Tree] = {
    import c.universe._
    
    State { argsLeft => 

      def popArg(f: c.Expr[Any] => c.Tree): (List[c.Expr[Any]], c.Tree) = argsLeft match {
        case a :: as => (as, f(a))
        case Nil => (Nil, c.abort(c.enclosingPosition, "Internal error: ran out of args"))
      }

      target match {
        case NamedTarget(name) => (argsLeft, q"${Ident(TermName(name))}") 
        case InterpTarget => popArg(a => q"($a)") 
      }
    }
  }


  private def interpretComposedLens(c: whitebox.Context)(clens: ComposedLens): State[List[c.Expr[Any]], c.Tree] = {
    import c.universe._

    type ArgState[A] = State[List[c.Expr[Any]], A]

    val treeState: ArgState[List[c.Tree]] = 
      clens.list.traverse[ArgState, c.Tree](lexpr => interpretLensExpr(c)(lexpr))


    treeState.map { trees => 
      val head :: tail = trees 

      tail.foldLeft(q"($head)"){ (accum, tr) => 
        val operators = List("composeLens", "composeIso", "composePrism",
                             "composeOptional", "composeGetter", "composeTraversal", 
                             "composeFold", "composeSetter")

        typeCheckUntilSomethingWorks(c)(
          operators.map(o => q"($accum).${TermName(o)}($tr)"): _*)
      }
    }
  }


  private def interpretLensExpr(c: whitebox.Context)(lexpr: LensExpr): State[List[c.Expr[Any]], c.Tree] = 
    State { argsLeft => 
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

  private def typeCheckUntilSomethingWorks(c: whitebox.Context)(trees: c.Tree*): c.Tree = 
    trees.find(t => c.typecheck(t, silent = true).nonEmpty).
      orElse(trees.headOption).
      getOrElse(c.abort(c.enclosingPosition, "Internal error: no trees to typecheck"))
   

  private def getContextStringParts(implicit c: whitebox.Context): List[String] = {
    import c.universe._

    c.prefix.tree match {
      case Apply(f, List(Apply(g, rawParts))) => rawParts.map {
        case Literal(Constant(str: String)) => str
      }
    }
  }
}
