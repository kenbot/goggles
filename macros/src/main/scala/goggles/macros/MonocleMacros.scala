package goggles.macros

import scala.reflect.macros.whitebox
import monocle._

import scala.util.control.NonFatal


object MonocleMacros {

  import scalaz._, Scalaz._
  import AST._

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
      interpretAppliedComposedLens(c)(acl, AccessMode.Get).flatMap(t => getterExpression(t)).eval(None, args.toList)
    }

    result match {
      case Left(err) => c.abort(c.enclosingPosition, err.toString)
      case Right(tree) => tree
    }
  }

  def setImpl(c: whitebox.Context)(args: c.Expr[Any]*): c.Tree = {
    import c.universe._
    import AST._

    val errorOrAst = Parser.parseAppliedLens(Lexer(getContextStringParts(c)))

    type Interpret[A] = Parse[Option[c.Type], c.Expr[Any], A]

    def setterExpression(tree: c.Tree): Interpret[c.Tree] = {
      val finalTree = getOpticType(c)(tree).map {
        case OpticType.SetterType => tree
        case _ => q"($tree).asSetter"
      }

      finalTree match {
        case Some(t) => Parse.result(t)
        case None => Parse.raiseError(SetNotAllowed(tree.toString))
      }
    }

    val resultState: Interpret[c.Tree] =
      for {
        applied <- errorOrAst.fold[Interpret[AppliedLens]](
                     e => Parse.raiseError(e),
                     t => Parse.result(t))
        tree <- interpretAppliedComposedLens(c)(applied, AccessMode.Set)
        setter <- setterExpression(tree)
      } yield {
        println(setter)
        q"(new _root_.goggles.macros.MonocleModifyOps($setter))"
      }

    resultState.eval(None, args.toList) match {
      case Left(err) => c.abort(c.enclosingPosition, err.toString)
      case Right(tree) => tree
    }
  }

  def lensImpl(c: whitebox.Context)(args: c.Expr[Any]*): c.Tree = {
    import c.universe._

    val errorOrAst = Parser.parseComposedLens(Lexer(getContextStringParts(c)))

    val resultState = errorOrAst.flatMap(ast => 
      interpretComposedLens(c)(ast, AccessMode.GetAndSet).eval(None, args.toList))

    resultState match {
      case Left(err) => c.abort(c.enclosingPosition, err.toString)
      case Right(tree) => tree
    }
  }


  private def getOpticType(c: whitebox.Context)(tree: c.Tree): Option[OpticType] = {
    import c.universe._

    c.typecheck(tree)

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
        case None => c.abort(c.enclosingPosition, s"Not an optic: ${show(ex.tree)}")
      }
    }
  }

  private def interpretAppliedComposedLens(c: whitebox.Context)(acl: AppliedLens, mode: AccessMode): Parse[Option[c.Type], c.Expr[Any], c.Tree] = {
    import c.universe._

    type Interpret[A] = Parse[Option[c.Type], c.Expr[Any], A]

    val treeState: Interpret[List[c.Expr[Any]]] =
      acl.lens.toList.traverse[Interpret, c.Expr[Any]](lexpr => interpretLensExpr(c)(lexpr, mode))

    for {
      sourceTree <- interpretSource(c)
      lensTrees <- treeState
    } yield composeLensTrees(c)(NonEmptyList(sourceTree, lensTrees: _*))
  }

  private def interpretComposedLens(c: whitebox.Context)(clens: ComposedLens, mode: AccessMode): Parse[Option[c.Type], c.Expr[Any], c.Tree] = {
    import c.universe._

    type Interpret[A] = Parse[Option[c.Type], c.Expr[Any], A]

    val treeState: Interpret[List[c.Expr[Any]]] =
      clens.toList.traverse[Interpret, c.Expr[Any]](lexpr => interpretLensExpr(c)(lexpr, mode))

    def nonEmptify[A](exprs: List[A]): Interpret[NonEmptyList[A]] = exprs match {
      case e :: es => Parse.result(NonEmptyList(e, es: _*))
      case Nil => Parse.raiseError(NoLensesProvided)
    }

    for {
      exprs <- treeState
      nonEmptyExprs <- nonEmptify(exprs)
    } yield composeLensTrees(c)(nonEmptyExprs)
  }


  private def interpretLensExpr(c: whitebox.Context)(lexpr: LensExpr, accessMode: AccessMode): Parse[Option[c.Type], c.Expr[Any], c.Expr[Any]] = {
    import c.universe._

    type Interpret[A] = Parse[Option[c.Type], c.Expr[Any], A]

    def setOutputType(t: c.Type): Interpret[Unit] =
      Parse.setState(Option(t))

    def typeCheckOrElse(tree: c.Tree, orElse: => ParseError): Interpret[c.Tree] =
      try Parse.result(c.typecheck(tree))
      catch { case NonFatal(_) => Parse.raiseError(orElse) }

    def patternMatchOrElse[A, R](a: A, orElse: => ParseError)(pf: PartialFunction[A,R]): Interpret[R] =
      if (pf.isDefinedAt(a)) Parse.result(pf(a))
      else Parse.raiseError(orElse)

    def checkOrElse[A](a: A, orElse: => ParseError)(p: A => Boolean): Interpret[A] =
      if (p(a)) Parse.result(a)
      else Parse.raiseError(orElse)

    def getInputTypeOrElse(orElse: => ParseError): Interpret[c.Type] =
      Parse.getState.flatMap {
        case Some(inputType) => Parse.result(inputType)
        case None => Parse.raiseError(orElse)
      }

    def isGetter(name: String)(sym: c.Symbol): Boolean =
      sym.name == TermName(name) && sym.isMethod && sym.asMethod.paramLists.flatten == Nil

    def interpretNamedRefGetter(name: String): Interpret[c.Expr[Any]] = {
      for {
        inputType <- getInputTypeOrElse(MissingInputType(name))
        getter <- checkOrElse(inputType.member(TermName(name)), InvalidGetter(name))(isGetter(name))
        outputType = getter.info
        _ <- setOutputType(outputType)
      } yield c.Expr(q"_root_.monocle.Getter[$inputType, $outputType](_.${TermName(name)})")
    }

    def interpretNamedRefGetterSetter(name: String): Interpret[c.Expr[Any]] = {
      for {
        inputType <- getInputTypeOrElse(MissingInputType(name))
        getter <- checkOrElse(inputType.member(TermName(name)), InvalidGetter(name))(isGetter(name))
        outputType = getter.info
        _ <- setOutputType(outputType)
      } yield c.Expr(q"_root_.monocle.Lens[$inputType, $outputType](_.${TermName(name)})(a => s => s.copy(${TermName(name)} = a))")
    }

    def interpretNamedRefSetter(name: String): Interpret[c.Expr[Any]] = {
      def getSetterType(sym: c.Symbol): Interpret[c.Type] = {
        if (sym.name != TermName("copy") ||
            !sym.isMethod ||
            sym.asMethod.paramLists.length != 1) {
          Parse.raiseError(InvalidSetter("Expecting copy method"))
        } else {
          val opt = sym.asMethod.paramLists.head.find(_.name == TermName(name)).map(_.info)

          opt match {
            case Some(t) => Parse.result(t)
            case None => Parse.raiseError(InvalidSetter(name))
          }
        }
      }

      for {
        inputType <- getInputTypeOrElse(MissingInputType(name))
        copyMethod = inputType.member(TermName("copy")).asMethod
        outputType <- getSetterType(copyMethod)
        _ <- setOutputType(outputType)
      } yield {
        c.Expr(q"_root_.monocle.Setter[$inputType, $outputType](f => s => s.copy(${TermName(name)} = f(s.${TermName(name)})))")
      }
    }

    def interpretInterpolatedRef: Interpret[c.Expr[Any]] = {
      for {
        lens <- Parse.popArg[Option[c.Type], c.Expr[Any]]
        outputType = lens.actualType.typeArgs(1)
        _ <- setOutputType(outputType)
      } yield lens
    }

    def interpretEach: Interpret[c.Expr[Any]] = {
      for {
        inputType <- getInputTypeOrElse(MissingInputType("*"))
        untypedTree = q"_root_.monocle.function.Each.each[$inputType, $WildcardType]"
        typedTree <- typeCheckOrElse(untypedTree, ImplicitEachNotFound(inputType.toString))
        outputType <- patternMatchOrElse(typedTree, ImplicitEachNotFound(inputType.toString)) {
                        case Apply(_, List(TypeApply(_, List(next)))) => next.tpe
                      }
        _ <- setOutputType(outputType)
      } yield c.Expr(typedTree)
    }

    def interpretOpt: Interpret[c.Expr[Any]] = {
      for {
        inputType <- getInputTypeOrElse(MissingInputType("?"))
        optionSymbol = c.symbolOf[scala.Option[_]]
        outputType <- patternMatchOrElse(inputType, OptionNotFound(inputType.toString)) {
                        case TypeRef(_, _, List(next)) => next
                      }
        _ <- setOutputType(outputType)
      } yield c.Expr(q"_root_.monocle.std.option.some[$outputType]")
    }

    def interpretIndex(i: c.Expr[Any]): Interpret[c.Expr[Any]] = {
      for {
        inputType <- getInputTypeOrElse(MissingInputType("[n]"))
        untypedTree = q"_root_.monocle.function.Index.index[$inputType, $WildcardType, $WildcardType]($i)"
        typedTree <- typeCheckOrElse(untypedTree, ImplicitIndexNotFound(inputType.toString))
        outputType <- patternMatchOrElse(typedTree, ImplicitIndexNotFound(inputType.toString)) {
          case Apply(_, List(TypeApply(_, List(next)))) => next.tpe
        }
        _ <- setOutputType(outputType)
      } yield c.Expr(typedTree)
    }

    lexpr match {
      case RefExpr(NamedLensRef(name)) => accessMode match {
        case AccessMode.Get => interpretNamedRefGetter(name)
        case AccessMode.Set => interpretNamedRefSetter(name)
        case AccessMode.GetAndSet => interpretNamedRefGetterSetter(name)
      }
      case RefExpr(InterpLensRef) => interpretInterpolatedRef
      case EachExpr => interpretEach
      case OptExpr => interpretOpt
      case IndexedExpr(LiteralIndex(i)) => interpretIndex(c.Expr(q"$i"))
      case IndexedExpr(InterpIndex) => Parse.popArg.flatMap(i => interpretIndex(i))
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
