package goggles.macros.interpret.features

import goggles.macros.interpret.infrastructure.{Contextual, OpticInfoContext, TypeCheckContext}
import goggles.macros.interpret.{Parse, OpticType}
import goggles.macros.errors.{UserError, InternalError}

trait IndexFeature {
  self: Contextual with OpticInfoContext 
                   with TypeCheckContext =>

  import c.universe._

  def interpretLiteralIndex(i: Int): Interpret[c.Tree] = 
    interpretIndex(q"$i", s"[$i]", typeOf[Int])

  def interpretInterpolatedIndex: Interpret[c.Tree] = 
    Parse.popArg.flatMap(i => interpretIndex(i.tree, s"[${getArgLabel(i.tree)}]", i.actualType))

  private def interpretIndex(i: c.Tree, label: String, indexType: c.Type): Interpret[c.Tree] = {
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
      typedTree <- typeCheckOrElse(untypedTree, UserError.ImplicitIndexNotFound(label, sourceType, indexType))
      targetType <- patternMatchOrElse(typedTree, InternalError.UnexpectedIndexStructure(sourceType, indexType)) {
        case ImplicitIndexTargetType(nextType) => nextType
      }
      _ <- storeOpticInfo(label, sourceType, targetType, OpticType.OptionalType)
    } yield q"_root_.monocle.function.Index.index($i)"
  }
}