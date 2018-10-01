package goggles.macros.interpret

import goggles.macros.errors._
import goggles.macros.lex.Lexer
import goggles.macros.parse.{AST, Parser}

import scala.reflect.macros.whitebox



private[goggles] object MacroInterpreter {

  import AST._
  import OpticType._

  def getImpl(c: whitebox.Context)(args: c.Expr[Any]*): MacroResult[c.Type, c.Tree] = {
    import AST._
    import c.universe._

    type Interpret[A] = Parse[c.Type, c.Expr[Any], A]

    def getterExpression(tree: c.Tree): Interpret[c.Tree] = {
      def mungePostFix(tree: c.Tree, pfix: Option[String]): c.Tree =
        pfix.fold(tree)(str => q"$tree.${TermName(str)}")

      for {
        info <- Parse.getLastOpticInfoOrElse[c.Type, c.Expr[Any]](OpticInfoNotFound(show(tree)))
        verb <- Parse.fromOption(info.compositeOpticType.getVerb, GetterOpticRequired
        (info.compositeOpticType))
        postFix = info.compositeOpticType.getVerbPostfix
      } yield mungePostFix(q"($tree).${TermName(verb)}(())", postFix)
    }

    val errorOrAst: Either[GogglesError[c.Type], AppliedLens] =
      Parser.parseAppliedLens(Lexer(getContextStringParts(c)))

    val finalTree =
      for {
        ast <- Parse.fromEither(errorOrAst)
        tree <- interpretComposedLens(c)(ast.lens, DslMode.Get)
        getter <- getterExpression(tree)
      } yield getter

    MacroResult.tupled(finalTree.eval(args.toList))
  }

  def setImpl(c: whitebox.Context)(args: c.Expr[Any]*): MacroResult[c.Type, c.Tree] = {
    import AST._
    import c.universe._

    type Interpret[A] = Parse[c.Type, c.Expr[Any], A]

    def setterExpression(tree: c.Tree): Interpret[c.Tree] = {
      for {
        info <- Parse.getLastOpticInfoOrElse[c.Type, c.Expr[Any]](OpticInfoNotFound(show(tree)))
        tree <- info.compositeOpticType match {
          case OpticType.SetterType => Parse.pure[c.Type, c.Expr[Any], c.Tree](tree)
          case x if x.allowsSet => Parse.pure[c.Type, c.Expr[Any], c.Tree](q"($tree).asSetter")
          case x => Parse.raiseError[c.Type, c.Expr[Any], c.Tree](SetterOpticRequired(x))
        }
      } yield tree
    }

    val errorOrAst: Either[GogglesError[c.Type], AppliedLens] =
      Parser.parseAppliedLens(Lexer(getContextStringParts(c)))

    val finalTree: Interpret[c.Tree] =
      for {
        ast <- Parse.fromEither(errorOrAst)
        tree <- interpretComposedLens(c)(ast.lens, DslMode.Set)
        setter <- setterExpression(tree)
      } yield q"(new _root_.goggles.macros.MonocleModifyOps($setter))"

    MacroResult.tupled(finalTree.eval(args.toList))
  }

  def lensImpl(c: whitebox.Context)(args: c.Expr[Any]*): MacroResult[c.Type, c.Tree] = {

    type Interpret[A] = Parse[c.Type, c.Expr[Any], A]

    val errorOrAst: Either[GogglesError[c.Type], ComposedLens] =
      Parser.parseUnappliedLens(Lexer(getContextStringParts(c)))

    val finalTree: Interpret[c.Tree] =
      for {
        ast <- Parse.fromEither(errorOrAst)
        tree <- interpretComposedLens(c)(ast, DslMode.Lens)
      } yield tree

    MacroResult.tupled(finalTree.eval(args.toList))
  }

  private def interpretComposedLens(c: whitebox.Context)(composedLens: ComposedLens, mode: DslMode): Parse[c.Type, c.Expr[Any], c.Tree] = {
    import c.universe._

    type Interpret[A] = Parse[c.Type, c.Expr[Any], A]

    def composeAll(code: c.Tree, lensExprs: List[LensExpr]): Interpret[c.Tree] = {
      def compose(codeSoFar: c.Tree, lensExpr: LensExpr): Interpret[c.Tree] = {
        for {
          lastInfo <- getLastOpticInfo(show(codeSoFar))
          nextLensCode <- interpretLensExpr(lensExpr)
          thisInfo <- getLastOpticInfo(show(nextLensCode))
          tree = q"($codeSoFar).${TermName(thisInfo.opticType.composeVerb)}($nextLensCode)"
          checkedTree <- typeCheckOrElse(tree, TypesDontMatch(thisInfo.label, thisInfo.sourceType, thisInfo.targetType, lastInfo.targetType, thisInfo.sourceType))
        } yield checkedTree
      }

      lensExprs match {
        case lx :: lxs => compose(code, lx).flatMap(composeAll(_, lxs))
        case Nil => Parse.pure(code)
      }
    }

    def typeCheckOrElse(tree: c.Tree, orElse: => GogglesError[c.Type]): Interpret[c.Tree] = {
      val typed = c.typecheck(tree, silent = true)
      if (typed.isEmpty) Parse.raiseError(orElse)
      else Parse.pure(typed)
    }

    def getArgLabel(tree: c.Tree): String = {
      val pos = tree.pos
      val src = new String(pos.source.content)
      val label = src.substring(pos.start, pos.end)
      if (label.forall(_.isUnicodeIdentifierPart)) s"$$$label"
      else s"$${$label}"
    }

    def interpretLensExpr(lexpr: LensExpr): Interpret[c.Tree] = {

      def patternMatchOrElse[A, R](a: A, orElse: => GogglesError[c.Type])(pf: PartialFunction[A,R]): Interpret[R] =
        if (pf.isDefinedAt(a)) Parse.pure(pf(a))
        else Parse.raiseError(orElse)

      def getLastTargetType(name: String): Interpret[c.Type] =
        getLastOpticInfo(name).map(_.targetType)

      def interpretNamedRefGetterSetter(name: String): Interpret[c.Tree] = {
        val label = s".$name"

        def validateGetter(sourceType: c.Type): Interpret[c.Type] = {
          val getter = sourceType.member(TermName(name))
          if (getter == NoSymbol) Parse.raiseError(NameNotFound(name, sourceType))
          else if (!getter.isMethod) Parse.raiseError(NameNotAMethod(name, sourceType))
          else {
            getter.asMethod.paramLists match {
              case Nil | List(Nil) => Parse.pure(getter.info)
              case List(_, _, _*) => Parse.raiseError(NameHasMultiParamLists(name, sourceType))
              case List(List(_, _*)) => Parse.raiseError(NameHasArguments(name, sourceType))
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
              case List(_, _, _*) => Parse.raiseError(CopyMethodHasMultiParamLists(name, sourceType))
              case List(args) if args.exists(!_.asTerm.isParamWithDefault) =>
                val argsWithNoDefaults = args.filterNot(_.asTerm.isParamWithDefault).map(_.name.toString)
                Parse.raiseError(CopyMethodLacksParameterDefaults(name, sourceType, argsWithNoDefaults))
              case List(args) if !args.exists(_.name == TermName(name)) =>
                Parse.raiseError(CopyMethodLacksNamedArgument(name, sourceType))
              case _ => Parse.pure(())
            }
          }
        }

        def validateSetterIfRequired(sourceType: c.Type): Interpret[Unit] = {
          if (!mode.isReadOnly) validateSetter(sourceType)
          else Parse.pure(())
        }

        for {
          sourceType <- getLastTargetType(name)
          targetType <- validateGetter(sourceType)
          _ <- validateSetterIfRequired(sourceType)
          _ <- storeOpticInfo(s".$name", sourceType, targetType, mode.opticType)
        } yield mode match {
          case DslMode.Get => q"_root_.monocle.Getter((s: $sourceType) => s.${TermName(name)})"
          case DslMode.Set => q"_root_.monocle.Setter[$sourceType, $targetType](f => s => s.copy(${TermName(name)} = f(s.${TermName(name)})))"
          case DslMode.Lens => q"_root_.monocle.Lens((s: $sourceType) => s.${TermName(name)})(a => (s: $sourceType) => s.copy(${TermName(name)} = a))"
        }
      }

      def interpretInterpolatedLens: Interpret[c.Tree] = {
        def getOpticTypeFromArg(argLabel: String, actualType: c.Type): Interpret[OpticType] = {
          actualType.erasure.typeSymbol.name.toString match {
            case "Fold" => Parse.pure(OpticType.FoldType)
            case "Getter" => Parse.pure(OpticType.GetterType)
            case "PSetter" => Parse.pure(OpticType.SetterType)
            case "PTraversal" => Parse.pure(OpticType.TraversalType)
            case "POptional" => Parse.pure(OpticType.OptionalType)
            case "PPrism" => Parse.pure(OpticType.PrismType)
            case "PLens" => Parse.pure(OpticType.LensType)
            case "PIso" => Parse.pure(OpticType.IsoType)
            case _ => Parse.raiseError(InterpNotAnOptic(argLabel, actualType))
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
          argLabel = getArgLabel(arg.tree)
          opticType <- getOpticTypeFromArg(argLabel, arg.actualType)
          io <- getInputOutputTypes(arg.actualType, opticType)
          (inType, outType) = io
          _ <- storeOpticInfo(s".$argLabel", inType, outType, opticType)
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

        val name = "*"
        for {
          sourceType <- getLastTargetType("*")
          untypedTree = q"implicitly[_root_.monocle.function.Each[$sourceType, _]]"
          typedTree <- typeCheckOrElse(untypedTree, ImplicitEachNotFound(name, sourceType))
          targetType <- patternMatchOrElse(typedTree, UnexpectedEachStructure) {
            case ImplicitEachTargetType(nextType) => nextType
          }
          _ <- storeOpticInfo(name, sourceType, targetType, TraversalType)
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
        val name = "?"
        for {
          sourceType <- getLastTargetType(name)
          untypedTree = q"implicitly[_root_.monocle.function.Possible[$sourceType, _]]"
          typedTree <- typeCheckOrElse(untypedTree, ImplicitPossibleNotFound(name, sourceType))
          targetType <- patternMatchOrElse(typedTree, UnexpectedPossibleStructure) {
            case ImplicitPossibleTargetType(nextType) => nextType
          }
          _ <- storeOpticInfo("?", sourceType, targetType, OptionalType)
        } yield q"_root_.monocle.function.Possible.possible"
      }

      def interpretIndex(i: c.Tree, label: String, indexType: c.Type): Interpret[c.Tree] = {
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

        for {
          sourceType <- getLastTargetType(label)
          untypedTree = q"implicitly[_root_.monocle.function.Index[$sourceType,$indexType,_]]"
          typedTree <- typeCheckOrElse(untypedTree, ImplicitIndexNotFound(label, sourceType, indexType))
          targetType <- patternMatchOrElse(typedTree, UnexpectedIndexStructure(sourceType, indexType)) {
            case ImplicitIndexTargetType(nextType) => nextType
          }
          _ <- storeOpticInfo(label, sourceType, targetType, OptionalType)
        } yield q"_root_.monocle.function.Index.index($i)"
      }


      lexpr match {
        case RefExpr(NamedLensRef(name)) => interpretNamedRefGetterSetter(name)
        case RefExpr(InterpLensRef) => interpretInterpolatedLens
        case EachExpr => interpretEach
        case OptExpr => interpretPossible
        case IndexedExpr(LiteralIndex(i)) => interpretIndex(q"$i", s"[$i]", typeOf[Int])
        case IndexedExpr(InterpIndex) => Parse.popArg.flatMap { i =>
          interpretIndex(i.tree, s"[${getArgLabel(i.tree)}]", i.actualType)
        }
      }
    }

    def getLastOpticInfo(name: String): Interpret[OpticInfo[c.Type]] = {
      Parse.getLastOpticInfo[c.Type, c.Expr[Any]].flatMap {
        case Some(info) => Parse.pure(info)
        case None => Parse.raiseError(OpticInfoNotFound(name))
      }
    }

    def storeOpticInfo(label: String, sourceType: c.Type, targetType: c.Type, opticType: OpticType): Interpret[Unit] = {
      for {
        lastInfo <- Parse.getLastOpticInfo[c.Type, c.Expr[Any]]
        nextOpticType = lastInfo match {
          case Some(info) => info.compositeOpticType.compose(opticType)
          case None => Some(opticType)
        }
        fromOptic = lastInfo.fold(opticType)(_.compositeOpticType)
        composed <- Parse.fromOption(nextOpticType,
                                     WrongKindOfOptic(label, sourceType, targetType, fromOptic, opticType))
        _ <- Parse.storeOpticInfo(OpticInfo(label, sourceType.resultType, targetType.resultType, opticType, composed))
      } yield ()
    }

    def interpretSource: Interpret[c.Tree] = {
      for {
        arg <- Parse.popArg[c.Type, c.Expr[Any]]
        _ <- Parse.storeOpticInfo(OpticInfo(getArgLabel(arg.tree), typeOf[Unit], arg.actualType, IsoType, IsoType))
      } yield q"_root_.goggles.macros.AppliedObject.const($arg)"
    }

    val (initCode, lensExprs) =
      if (mode.appliedToObject) (interpretSource, composedLens.toList)
      else (interpretLensExpr(composedLens.head), composedLens.tail)

    initCode.flatMap(composeAll(_, lensExprs))
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
