package goggles.macros.interpret.features

import goggles.macros.interpret._
import goggles.macros.errors._

trait NamedLensRefFeature {
  self: Contextual with DslModeContext 
                   with InterpreterTools =>

  import c.universe._

  def interpretNamedLensRef(name: String): Interpret[c.Tree] = {
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
}