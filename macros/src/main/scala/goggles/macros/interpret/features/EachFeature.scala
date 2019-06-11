package goggles.macros.interpret.features

import goggles.macros.interpret.infrastructure.{Contextual, InterpreterActions}
import goggles.macros.interpret.OpticType
import goggles.macros.errors.{UserError, InternalError}

trait EachFeature {
  self: Contextual with InterpreterActions =>

  import c.universe._

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
      typedTree <- typeCheckOrElse(untypedTree, UserError.ImplicitEachNotFound(name, sourceType))
      targetType <- patternMatchOrElse(typedTree, InternalError.UnexpectedEachStructure) {
        case ImplicitEachTargetType(nextType) => nextType
      }
      _ <- storeOpticInfo(name, sourceType, targetType, OpticType.TraversalType)
    } yield q"_root_.monocle.function.Each.each"
  }
}