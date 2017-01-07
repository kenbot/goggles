package goggles.macros

import goggles.macros.OpticType._

import scala.reflect.macros.whitebox
import monocle._


object GogglesMacros {

  import AST._

  def getImpl(c: whitebox.Context)(args: c.Expr[Any]*): c.Tree = {
    import c.universe._
    import AST._

    type Interpret[A] = Parse[c.Type, c.Expr[Any], A]

    def getterExpression(tree: c.Tree): Interpret[c.Tree] = {
      for {
        info <- Parse.getLastParseInfoOrElse[c.Type, c.Expr[Any]](ParseInfoNotFound(show(tree)))
        verb <- Parse.fromOption(info.compositeOpticType.getVerb, GetVerbNotFound(info.compositeOpticType))
      } yield q"($tree).${TermName(verb)}(())"
    }

    val errorOrAst: Either[GogglesError, AppliedLens] =
      Parser.parseAppliedLens(Lexer(getContextStringParts(c)))

    val finalTree =
      for {
        ast <- Parse.fromEither(errorOrAst)
        tree <- interpretComposedLens(c)(ast.lens, applied = true)
        getter <- getterExpression(tree)
      } yield getter

    val (errorOrTree, infos) = finalTree.eval(args.toList)

    errorOrTree match {
      case Left(err) => c.abort(c.enclosingPosition, err.toString)
      case Right(tree) => tree
    }
  }

  def setImpl(c: whitebox.Context)(args: c.Expr[Any]*): c.Tree = {
    import c.universe._
    import AST._

    type Interpret[A] = Parse[c.Type, c.Expr[Any], A]

    def setterExpression(tree: c.Tree): Interpret[c.Tree] = {
      for {
        info <- Parse.getLastParseInfoOrElse[c.Type, c.Expr[Any]](ParseInfoNotFound(show(tree)))
      } yield info.compositeOpticType match {
        case OpticType.SetterType => tree
        case x => q"($tree).asSetter"
      }
    }

    val errorOrAst: Either[GogglesError, AppliedLens] =
      Parser.parseAppliedLens(Lexer(getContextStringParts(c)))

    val finalTree: Interpret[c.Tree] =
      for {
        ast <- Parse.fromEither(errorOrAst)
        tree <- interpretComposedLens(c)(ast.lens, applied = true)
        setter <- setterExpression(tree)
      } yield q"(new _root_.goggles.macros.MonocleModifyOps($setter))"

    val (errorOrTree, infos) = finalTree.eval(args.toList)
    errorOrTree match {
      case Left(err) => c.abort(c.enclosingPosition, err.toString)
      case Right(tree) => tree
    }
  }

  def lensImpl(c: whitebox.Context)(args: c.Expr[Any]*): c.Tree = {
    import c.universe._

    type Interpret[A] = Parse[c.Type, c.Expr[Any], A]

    val errorOrAst: Either[GogglesError, ComposedLens] =
      Parser.parseUnappliedLens(Lexer(getContextStringParts(c)))

    val finalTree: Interpret[c.Tree] =
      for {
        ast <- Parse.fromEither(errorOrAst)
        tree <- interpretComposedLens(c)(ast, applied = false)
      } yield tree

    val (errorOrTree, infos) = finalTree.eval(args.toList)

    errorOrTree match {
      case Left(err) => c.abort(c.enclosingPosition, err.toString)
      case Right(tree) => tree
    }
  }

  private def interpretComposedLens(c: whitebox.Context)(composedLens: ComposedLens, applied: Boolean): Parse[c.Type, c.Expr[Any], c.Tree] = {
    import c.universe._

    type Interpret[A] = Parse[c.Type, c.Expr[Any], A]

    def composeAll(code: c.Tree, lensExprs: List[LensExpr]): Interpret[c.Tree] = {
      def compose(codeSoFar: c.Tree, lensExpr: LensExpr): Interpret[c.Tree] = {
        for {
          nextLensCode <- interpretLensExpr(lensExpr)
          lastInfo <- getLastParseInfo(show(codeSoFar))
        } yield q"($codeSoFar).${TermName(lastInfo.opticType.composeVerb)}($nextLensCode)"
      }

      lensExprs match {
        case lx :: lxs => compose(code, lx).flatMap(composeAll(_, lxs))
        case Nil => Parse.pure(code)
      }
    }


    def interpretLensExpr(lexpr: LensExpr): Interpret[c.Tree] = {

      def patternMatchOrElse[A, R](a: A, orElse: => GogglesError)(pf: PartialFunction[A,R]): Interpret[R] =
        if (pf.isDefinedAt(a)) Parse.pure(pf(a))
        else Parse.raiseError(orElse)

      def getLastTargetType(name: String): Interpret[c.Type] =
        getLastParseInfo(name).map(_.targetType)

      def typeCheckOrElse(tree: c.Tree, orElse: => GogglesError): Interpret[c.Tree] = {
        val typed = c.typecheck(tree, silent = true)
        if (typed.isEmpty) Parse.raiseError(orElse)
        else Parse.pure(typed)
      }

      def interpretNamedRefGetterSetter(name: String): Interpret[c.Tree] = {
        def validateGetter(sourceType: c.Type): Interpret[c.Type] = {
          val getter = sourceType.member(TermName(name))
          if (getter == NoSymbol) Parse.raiseError(NameNotFound(name, sourceType))
          else if (!getter.isMethod) Parse.raiseError(NameNotAMethod(name, sourceType))
          else {
            getter.asMethod.paramLists match {
              case List(_, _, _*) =>  Parse.raiseError(NameHasMultiParamLists(name, sourceType))
              case List(List(_, _*)) => Parse.raiseError(NameHasArguments(name, sourceType))
              case Nil | List(Nil) => Parse.pure(getter.info)
            }
          }
        }

        def validateSetter(sourceType: c.Type): Interpret[Unit] = {
          val copyMethod = sourceType.member(TermName("copy"))
          if (copyMethod == NoSymbol) Parse.raiseError(CopyMethodNotFound(name, sourceType))
          else if (!copyMethod.isMethod) Parse.raiseError(CopyMethodNotAMethod(name, sourceType))
          else {
            copyMethod.asMethod.paramLists match {
              case Nil | List(Nil) => Parse.raiseError(CopyMethodHasNoArguments(name, sourceType))
              case List(_, _, _*) =>  Parse.raiseError(CopyMethodHasMultiParamLists(name, sourceType))
              case List(args) if !args.exists(_.name == TermName(name)) => Parse.raiseError(CopyMethodLacksNamedArgument(name, sourceType))
              case List(args) if args.exists(!_.asTerm.isParamWithDefault) =>
                val argsWithNoDefaults = args.filterNot(_.asTerm.isParamWithDefault).map(_.name.toString)
                Parse.raiseError(CopyMethodLacksParameterDefaults(name, sourceType, argsWithNoDefaults))
              case _ => Parse.pure(())
            }
          }
        }

        for {
          sourceType <- getLastTargetType(name)
          targetType <- validateGetter(sourceType)
          _ <- validateSetter(sourceType)
          _ <- storeParseInfo(s".$name", sourceType, targetType, LensType)
        } yield q"_root_.monocle.Lens((s: $sourceType) => s.${TermName(name)})(a => (s: $sourceType) => s.copy(${TermName(name)} = a))"
      }

      def interpretInterpolatedLens: Interpret[c.Tree] = {
        def getOpticTypeFromArg(actualType: c.Type): Interpret[OpticType] = {
          actualType.erasure.typeSymbol.name.toString match {
            case "Fold" => Parse.pure(OpticType.FoldType)
            case "Getter" => Parse.pure(OpticType.GetterType)
            case "PSetter" => Parse.pure(OpticType.SetterType)
            case "PTraversal" => Parse.pure(OpticType.TraversalType)
            case "POptional" => Parse.pure(OpticType.OptionalType)
            case "PPrism" => Parse.pure(OpticType.PrismType)
            case "PLens" => Parse.pure(OpticType.LensType)
            case "PIso" => Parse.pure(OpticType.IsoType)
            case n => Parse.raiseError(InterpNotAnOptic(n, actualType))
          }
        }

        def getInputOutputTypes(actualType: c.Type, opticType: OpticType): Interpret[(c.Type, c.Type)] = {
          val typeArgs = actualType.typeArgs
          if (opticType.polymorphic && typeArgs.length == 4) {
            Parse.pure((typeArgs(0), typeArgs(2)))
          } else if (typeArgs.length == 2) {
            Parse.pure((typeArgs(0), typeArgs(1)))
          } else {
            Parse.raiseError(UnexpectedOpticKind(actualType, typeArgs.length))
          }
        }

        for {
          arg <- Parse.popArg[c.Type, c.Expr[Any]]
          opticType <- getOpticTypeFromArg(arg.actualType)
          io <- getInputOutputTypes(arg.actualType, opticType)
          (inType, outType) = io
          _ <- storeParseInfo(s"$${${show(arg.tree)}}", inType, outType, opticType)
        } yield arg.tree
      }

      def interpretEach: Interpret[c.Tree] = {
        object ImplicitEachTargetType {
          object InnerType {
            def unapply(t: c.Tree): Option[c.Type] = t.tpe match {
              case TypeRef(_, _, List(_, TypeRef(_, sym, _))) => Some(sym.asType.toType)
              case _ => None
            }
          }
          def unapply(tree: c.Tree): Option[c.Type] = tree match {
            case Apply(_, List(InnerType(next))) => Some(next)
            case _ => None
          }
        }

        for {
          sourceType <- getLastTargetType("*")
          untypedTree = q"implicitly[_root_.monocle.function.Each[$sourceType, _]]"
          typedTree <- typeCheckOrElse(untypedTree, ImplicitEachNotFound(sourceType))
          targetType <- patternMatchOrElse(typedTree, UnexpectedEachStructure) {
            case ImplicitEachTargetType(nextType) => nextType
          }
          _ <- storeParseInfo("*", sourceType, targetType, TraversalType)
        } yield q"_root_.monocle.function.Each.each"
      }

      def interpretPossible: Interpret[c.Tree] = {
        object ImplicitPossibleTargetType {
          object InnerType {
            def unapply(t: c.Tree): Option[c.Type] = t.tpe match {
              case TypeRef(_, _, List(_, TypeRef(_, sym, _))) => Some(sym.asType.toType)
              case _ => None
            }
          }
          def unapply(tree: c.Tree): Option[c.Type] = tree match {
            case Apply(_, List(InnerType(next))) => Some(next)
            case _ => None
          }
        }
        for {
          sourceType <- getLastTargetType("?")
          untypedTree = q"implicitly[_root_.monocle.function.Possible[$sourceType, _]]"
          typedTree <- typeCheckOrElse(untypedTree, ImplicitPossibleNotFound(sourceType))
          targetType <- patternMatchOrElse(typedTree, UnexpectedPossibleStructure) {
            case ImplicitPossibleTargetType(nextType) => nextType
          }
          _ <- storeParseInfo("?", sourceType, targetType, OptionalType)
        } yield q"_root_.monocle.function.Possible.possible"
      }

      def interpretIndex(i: c.Tree): Interpret[c.Tree] = {
        object ImplicitIndexTargetType {
          object InnerType {
            def unapply(t: c.Tree): Option[c.Type] = t.tpe match {
              case TypeRef(_, _, List(_, _, TypeRef(_, sym, _))) => Some(sym.asType.toType)
              case _ => None
            }
          }
          def unapply(tree: c.Tree): Option[c.Type] = tree match {
            case Apply(_, List(InnerType(next))) => Some(next)
            case _ => None
          }
        }

        val label = s"[${show(i)}]"
        for {
          sourceType <- getLastTargetType(label)
          untypedTree = q"implicitly[_root_.monocle.function.Index[$sourceType,_,_]]"
          typedTree <- typeCheckOrElse(untypedTree, ImplicitIndexNotFound(sourceType))
          targetType <- patternMatchOrElse(typedTree, UnexpectedIndexStructure) {
            case ImplicitIndexTargetType(nextType) => nextType
          }
          _ <- storeParseInfo(label, sourceType, targetType, OptionalType)
        } yield q"_root_.monocle.function.Index.index($i)"
      }

      lexpr match {
        case RefExpr(NamedLensRef(name)) => interpretNamedRefGetterSetter(name)
        case RefExpr(InterpLensRef) => interpretInterpolatedLens
        case EachExpr => interpretEach
        case OptExpr => interpretPossible
        case IndexedExpr(LiteralIndex(i)) => interpretIndex(q"$i")
        case IndexedExpr(InterpIndex) => Parse.popArg.flatMap(i => interpretIndex(i.tree))
      }
    }

    def getLastParseInfo(name: String): Interpret[ParseInfo[c.Type]] = {
      Parse.getLastParseInfo[c.Type, c.Expr[Any]].flatMap {
        case Some(info) => Parse.pure(info)
        case None => Parse.raiseError(ParseInfoNotFound(name))
      }
    }

    def storeParseInfo(name: String, inType: c.Type, outType: c.Type, opticType: OpticType): Interpret[Unit] = {
      for {
        lastInfo <- Parse.getLastParseInfo[c.Type, c.Expr[Any]]
        nextOpticType = lastInfo match {
          case Some(info) => info.compositeOpticType.compose(opticType)
          case None => Some(opticType)
        }
        composed <- Parse.fromOption(nextOpticType,
                                     WrongKindOfOptic(lastInfo.fold(opticType)(_.compositeOpticType), opticType))
        _ <- Parse.storeParseInfo(ParseInfo(name, inType, outType, opticType, composed))
      } yield ()
    }

    def interpretSource: Interpret[c.Tree] = {
      for {
        arg <- Parse.popArg[c.Type, c.Expr[Any]]
        _ <- Parse.storeParseInfo(ParseInfo(s"$$arg", typeOf[Unit], arg.actualType, IsoType, IsoType))
      } yield q"_root_.goggles.macros.GogglesMacros.const($arg)"
    }

    val (initCode, lensExprs) =
      if (applied) (interpretSource, composedLens.toList)
      else (interpretLensExpr(composedLens.head), composedLens.tail)

    initCode.flatMap(composeAll(_, lensExprs))
  }

  def const[A,B](a: => A) = PIso[Unit, B, A ,B](_ => a)(identity)

  private def getContextStringParts(implicit c: whitebox.Context): List[String] = {
    import c.universe._

    c.prefix.tree match {
      case Apply(f, List(Apply(g, rawParts))) => rawParts.map {
        case Literal(Constant(str: String)) => str
      }
    }
  }
}
