package goggles.macros.interpret.features

import goggles.macros.interpret._
import goggles.macros.errors._

trait PossibleFeature {
  self: Contextual with InterpreterTools =>

  import c.universe._

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
      _ <- storeOpticInfo("?", sourceType, targetType, OpticType.OptionalType)
    } yield q"_root_.monocle.function.Possible.possible"
  }
}