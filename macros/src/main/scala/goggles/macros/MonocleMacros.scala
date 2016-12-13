package goggles.macros

import scala.reflect.macros.whitebox
import monocle._

import scala.util.control.NonFatal


object MonocleMacros {

  import scalaz._, Scalaz._
  import AST._

  def setImpl(c: whitebox.Context)(args: c.Expr[Any]*): c.Tree = {
    /*
    import c.universe._
    import AST._

    val errorOrAst = Parser.parseAppliedLens(Lexer(getContextStringParts(c)))

    val resultState = errorOrAst.map { acl =>
      interpretAppliedComposedLens(c)(acl).map { tree =>
        q"(new _root_.goggles.macros.MonocleModifyOps(($tree).asSetter))"
      }
    }

    resultState match {
      case Left(err) => c.abort(c.enclosingPosition, err.toString)
      case Right(tree) => q"${tree.eval(args.toList)}"
    }
    */
    ???
  }

  def lensImpl(c: whitebox.Context)(args: c.Expr[Any]*): c.Tree = {
    import c.universe._

    val errorOrAst = Parser.parseComposedLens(Lexer(getContextStringParts(c)))

    val resultState = errorOrAst.flatMap(ast => 
      interpretComposedLens(c)(ast).eval(None, args.toList))

    resultState match {
      case Left(err) => c.abort(c.enclosingPosition, err.toString)
      case Right(tree) => tree
    }
  }

  def getImpl(c: whitebox.Context)(args: c.Expr[Any]*): c.Tree = {
    import c.universe._
    import AST._

    val errorOrAst = Parser.parseAppliedLens(Lexer(getContextStringParts(c)))

    type Interpret[A] = Parse[Option[c.Type], c.Expr[Any], A]

    def getterExpression(tree: c.Tree): Interpret[c.Tree] = {
      val finalTree = for {
        o <- getOpticType(c)(tree)
        verb <- o.getVerb
      } yield q"($tree).${TermName(verb)}(())"

      finalTree match {
        case Some(t) => Parse.result(t)
        case None => Parse.raiseError(GetNotAllowed(tree.toString))
      }
    }

    val result = errorOrAst.flatMap { acl =>
      interpretAppliedComposedLens(c)(acl).flatMap(t => getterExpression(t)).eval(None, args.toList)
    }

    result match {
      case Left(err) => c.abort(c.enclosingPosition, err.toString)
      case Right(tree) => tree
    }
  }

  private sealed abstract class OpticType(
    val typeName: String,
    val polymorphic: Boolean,
    val composeVerb: String,
    val getVerb: Option[String])

  private object OpticType {
    val all = List(FoldType, GetterType, SetterType,
                   TraversalType, OptionalType, PrismType,
                   LensType, IsoType)

    case object FoldType extends OpticType("Fold", false, "composeFold", Some("getAll"))
    case object GetterType extends OpticType("Getter", false, "composeGetter", Some("get"))
    case object SetterType extends OpticType("PSetter", true, "composeSetter", None)
    case object TraversalType extends OpticType("PTraversal", true, "composeTraversal", Some("getAll"))
    case object OptionalType extends OpticType("POptional", true, "composeOptional", Some("getOption"))
    case object PrismType extends OpticType("PPrism", true, "composePrism", Some("getOption"))
    case object LensType extends OpticType("PLens", true, "composeLens", Some("get"))
    case object IsoType extends OpticType("PIso", true, "composeIso", Some("get"))
  }

  private def getOpticType(c: whitebox.Context)(tree: c.Tree): Option[OpticType] = {
    import c.universe._

    OpticType.all.find { o =>
      val t = if (o.polymorphic) tq"_root_.monocle.${TypeName(o.typeName)}[_,_,_,_]"
              else tq"_root_.monocle.${TypeName(o.typeName)}[_,_]"

      c.typecheck(q"($tree): $t", silent = true).nonEmpty
    }
  }


  private def interpretSource(c: whitebox.Context): Parse[Option[c.Type], c.Expr[Any], c.Expr[Any]] = {
    import c.universe._

    for {
      arg <- Parse.popArg
      _ <- Parse.setState(Option(arg.actualType))
    } yield c.Expr(q"_root_.goggles.macros.MonocleMacros.const($arg)")
  }

  def const[A,B](a: => A) = PIso[Unit, B, A ,B](_ => a)(identity)

  private def composeLensTrees(c: whitebox.Context)(exprs: NonEmptyList[c.Expr[Any]]): c.Tree = {
    import c.universe._
    import scalaz._, Scalaz._

    val NonEmptyList(head, tail) = exprs

    tail.foldLeft(q"($head)") { (accum, ex) =>
      getOpticType(c)(ex.tree) match {
        case Some(o) => q"($accum).${TermName(o.composeVerb)}($ex)"
        case None => println(ex.tree); c.abort(c.enclosingPosition, s"Not an optic: ${show(ex.tree)}")
      }
    }
  }

  private def interpretAppliedComposedLens(c: whitebox.Context)(acl: AppliedLens): Parse[Option[c.Type], c.Expr[Any], c.Tree] = {
    import c.universe._

    type Interpret[A] = Parse[Option[c.Type], c.Expr[Any], A]

    val treeState: Interpret[List[c.Expr[Any]]] =
      acl.lens.toList.traverse[Interpret, c.Expr[Any]](lexpr => interpretLensExpr(c)(lexpr))

    for {
      sourceTree <- interpretSource(c)
      lensTrees <- treeState
    } yield composeLensTrees(c)(NonEmptyList(sourceTree, lensTrees: _*))
  }

  private def interpretComposedLens(c: whitebox.Context)(clens: ComposedLens): Parse[Option[c.Type], c.Expr[Any], c.Tree] = {
    import c.universe._

    type Interpret[A] = Parse[Option[c.Type], c.Expr[Any], A]

    val treeState: Interpret[List[c.Expr[Any]]] =
      clens.toList.traverse[Interpret, c.Expr[Any]](lexpr => interpretLensExpr(c)(lexpr))

    def nonEmptify[A](exprs: List[A]): Interpret[NonEmptyList[A]] = exprs match {
      case e :: es => Parse.result(NonEmptyList(e, es: _*))
      case Nil => Parse.raiseError(NoLensesProvided)
    }

    for {
      exprs <- treeState
      nonEmptyExprs <- nonEmptify(exprs)
    } yield composeLensTrees(c)(nonEmptyExprs)
  }

  private def typedLensExpr(c: whitebox.Context)(lens: c.Expr[Any]): Parse[Option[c.Type], c.Expr[Any], c.Expr[Any]] = {
    import c.universe._
    println(lens)
    Parse.setState(Option(lens.actualType.typeArgs(1))).map(_ => lens)
  }

  private def popTypedLensExpr(c: whitebox.Context): Parse[Option[c.Type], c.Expr[Any], c.Expr[Any]] = {
    import c.universe._
    for {
      arg <- Parse.popArg
      expr  <- typedLensExpr(c)(arg)
    } yield c.Expr(q"($expr)")
  }

  private def interpretLensExpr(c: whitebox.Context)(lexpr: LensExpr): Parse[Option[c.Type], c.Expr[Any], c.Expr[Any]] = {
    import c.universe._

    lexpr match {
      case RefExpr(NamedLensRef(name)) => Parse.getState.flatMap {
        case Some(inputType) =>
          val nextType = inputType.member(TermName(name)).info
          Parse.setState(Option(nextType)).map { _ =>
            c.Expr(q"_root_.monocle.Getter[$inputType, $WildcardType](_.${TermName(name)})")
          }

        case None => Parse.raiseError(NamedLensWithoutType(name))
      }

      case RefExpr(InterpLensRef) => popTypedLensExpr(c)
      case EachExpr =>
        Parse.getState[Option[c.Type], c.Expr[Any]].flatMap {
          case Some(inputType) =>
            val tree = c.typecheck(q"_root_.monocle.function.Each.each[$inputType, $WildcardType]")
            val nextType = tree match {
              case Apply(_, List(TypeApply(_, List(next)))) => next.tpe
            }
            Parse.setState(Option(nextType)).map(_ => c.Expr(tree))

          case None => Parse.raiseError(NamedLensWithoutType("*"))
        }

      case OptExpr => typedLensExpr(c)(c.Expr(q"_root_.monocle.std.option.pSome"))
      case IndexedExpr(LiteralIndex(i)) => typedLensExpr(c)(c.Expr(q"_root_.monocle.function.Index.index(${Literal(Constant(i))})"))
      case IndexedExpr(InterpIndex) =>
        popTypedLensExpr(c).map(e => c.Expr(q"_root_.monocle.function.Index.index($e)"))
    }
  }

  private def getContextStringParts(implicit c: whitebox.Context): List[String] = {
    import c.universe._

    c.prefix.tree match {
      case Apply(f, List(Apply(g, rawParts))) => rawParts.map {
        case Literal(Constant(str: String)) => str
      }
    }
  }
}
