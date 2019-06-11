package goggles.macros.interpret.features

import goggles.macros.interpret.infrastructure.{Contextual, InterpreterActions}
import goggles.macros.interpret.{Parse, OpticType}
import goggles.macros.errors.{InternalError, UserError}

trait InterpolatedLensRefFeature {
  self: Contextual with InterpreterActions =>

  def interpretInterpolatedLens: Interpret[c.Tree] = {
    for {
      arg <- Parse.popArg[c.Type, c.Expr[Any]]
      argLabel = getArgLabel(arg.tree)
      opticType <- getOpticTypeFromArg(argLabel, arg.actualType)
      io <- getInputOutputTypes(arg.actualType, opticType)
      (inType, outType) = io
      _ <- storeOpticInfo(s".$argLabel", inType, outType, opticType)
    } yield arg.tree
  }

  private def getOpticTypeFromArg(argLabel: String, actualType: c.Type): Interpret[OpticType] = {
    actualType.erasure.typeSymbol.name.toString match {
      case "Fold" => Parse.pure(OpticType.FoldType)
      case "Getter" => Parse.pure(OpticType.GetterType)
      case "PSetter" => Parse.pure(OpticType.SetterType)
      case "PTraversal" => Parse.pure(OpticType.TraversalType)
      case "POptional" => Parse.pure(OpticType.OptionalType)
      case "PPrism" => Parse.pure(OpticType.PrismType)
      case "PLens" => Parse.pure(OpticType.LensType)
      case "PIso" => Parse.pure(OpticType.IsoType)
      case _ => Parse.raiseError(UserError.InterpNotAnOptic(argLabel, actualType))
    }
  }

  private def getInputOutputTypes(actualType: c.Type, opticType: OpticType): Interpret[(c.Type, c.Type)] = {
    val typeArgs = actualType.typeArgs
    if (opticType.polymorphic && typeArgs.length == 4) {
      Parse.pure((typeArgs(0), typeArgs(2)))
    } else if (typeArgs.length == 2) {
      Parse.pure((typeArgs(0), typeArgs(1)))
    } else {
      Parse.raiseError(InternalError.UnexpectedOpticKind(actualType, typeArgs.length))
    }
  }
}