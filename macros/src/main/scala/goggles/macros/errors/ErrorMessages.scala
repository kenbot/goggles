package goggles.macros.errors

import goggles.macros._
import goggles.macros.interpret.{OpticInfo, DslMode}

import scala.reflect.api.Universe

object ErrorMessages {

  def message(error: GogglesError[Universe#Type], mode: DslMode, info: List[OpticInfo[Universe#Type]]): String = {

    def getSyntaxErrorMessage(e: SyntaxError): String = {
      def usage = mode match {
        case DslMode.Get => usageGet
        case DslMode.Set => usageSet
        case DslMode.Lens => usageLens
      }

      def syntaxError(msg: String) =
        s"$msg \n$usage"

      e match {
        case UnrecognisedChar(char) => syntaxError(s"unrecognised character: $char")
        case EmptyError => syntaxError("expression is empty.")
        case NameWithNoDot(name) => syntaxError(s"a dot is required before a field name: $name")
        case InterpOpticWithNoDot => syntaxError("a dot is required before an interpolated optic")
        case InvalidAfterDot(tok) => syntaxError(s"expecting a field name or interpolated optic after a dot, found: '" + tok + "'")
        case NonInterpolatedStart(tok) => syntaxError("expecting an interpolated value at the start, found: " + tok)
        case UnexpectedCloseBracket => syntaxError("unexpected \"]\"")
        case EndingDot => syntaxError("a dot must be followed either by a case class-like field or an interpolated optic")
        case NoIndexSupplied => syntaxError("expecting a supplied index value inside brackets []")
        case InvalidIndexSupplied(tok) => syntaxError("expecting an integer literal or interpolated index, found: \"" + tok + "\"")
        case UnclosedOpenBracket => syntaxError("bracket was never closed")
        case VerbatimIndexNotInt(expr) => syntaxError("verbatim index needs to be a positive int, eg. [0] or [1]")
      }
    }

    def getMacroUserErrorMessage(e: MacroUserError[Universe#Type]): String = {
      def userError(msg: String) = {
        val opticTable = TypeTableErrors.table(mode, e, info).render
        s"$msg \n$opticTable"
      }

      e match {
        case GetterOpticRequired(finalOpticType) => userError(s"Required an optic type that can get values; found ${finalOpticType.monoTypeName}")
        case SetterOpticRequired(finalOpticType) => userError(s"Required an optic type that can set values; found ${finalOpticType.monoTypeName}")
        case NameNotFound(name, sourceType) => userError(s"$sourceType doesn't have a '$name' method")
        case NameNotAMethod(name, sourceType) => userError(s"$sourceType member '$name' is not a method")
        case NameHasArguments(name, sourceType) => userError(s"'$name' method on $sourceType is not a valid getter; it has arguments")
        case NameHasMultiParamLists(name, sourceType) => userError(s"'$name' method on $sourceType is not a valid getter; it has multiple parameter lists")
        case InterpNotAnOptic(name, actualType) => userError(s"Interpolated value $name must be an optic; one of monocle.{Fold, Getter, Setter, Traversal, Optional, Prism, Lens, Iso}; found: $actualType")
        case WrongKindOfOptic(name, sourceType, targetType, from, to) => userError(s"Composition from ${from.article} ${from.monoTypeName} to ${to.article} ${to.monoTypeName} is not permitted in $name")
        case TypesDontMatch(name, sourceType, targetType, expectedType, actualType) => userError(s"The types of consecutive sections don't match.\n found   : $actualType\n required: $expectedType")
        case ImplicitEachNotFound(name, sourceType) => userError(s"No implicit monocle.function.Each[$sourceType, _] found to support '*' traversal")
        case ImplicitPossibleNotFound(name, sourceType) => userError(s"No implicit monocle.function.Possible[$sourceType, _] found to support '?' selection")
        case ImplicitIndexNotFound(name, sourceType, indexType) => userError(s"No implicit monocle.function.Index[$sourceType, $indexType, _] found to support '[]' indexing")
        case CopyMethodNotFound(name, sourceType) => userError(s"Can't update '$name', because no 'copy' method found on $sourceType")
        case CopyMethodNotAMethod(name, sourceType) => userError(s"Can't update '$name', because the $sourceType 'copy' member is not a method.")
        case CopyMethodHasMultiParamLists(name, sourceType) => userError(s"Can't update '$name', because the $sourceType 'copy' method has multiple parameter lists.")
        case CopyMethodHasNoArguments(name, sourceType) => userError(s"Can't update '$name', because the $sourceType 'copy' method has no arguments.")
        case CopyMethodLacksNamedArgument(name, sourceType) => userError(s"Can't update '$name', because the $sourceType 'copy' method doesn't have a parameter named '$name'.")
        case CopyMethodLacksParameterDefaults(name, sourceType, argsWithNoDefault) => userError(s"Can't update '$name', because the $sourceType 'copy' method doesn't have defaults defined for the following arguments: ${argsWithNoDefault.mkString(", ")}")
      }
    }

    def getMacroInternalErrorMessage(e: MacroInternalError[Universe#Type]): String = {
      def internalError(msg: String): String =
        s"$msg \nPlease consider filing an issue at https://github.com/kenbot/goggles/issues."

      e match {
        case OpticInfoNotFound(label) => internalError(s"The macro internally failed to track type information at '$label'.")
        case UnexpectedEachStructure => internalError("The macro internally failed to parse monocle.function.Each structure.")
        case UnexpectedPossibleStructure => internalError("The macro internally failed to parse monocle.function.Possible structure.")
        case UnexpectedIndexStructure(sourceType, indexType) => internalError(s"The macro internally failed to parse monocle.function.Index[$sourceType, $indexType, _] structure.")
        case UnexpectedOpticKind(actualType, numTypeArgs) => internalError(s"The macro internally failed to parse optic $actualType; found $numTypeArgs type arguments.")
        case GetVerbNotFound(opticType) => internalError(s"The macro internally failed to get values from optic ${opticType.monoTypeName}.")
        case NotEnoughArguments => internalError("The macro internally failed to extract the interpolated arguments.")
      }
    }

    error match {
      case e: SyntaxError => getSyntaxErrorMessage(e)
      case e: MacroUserError[_] => getMacroUserErrorMessage(e)
      case e: MacroInternalError[_] => getMacroInternalErrorMessage(e)
    }
  }

  def usageGet =
    """
      |    Goggles usage (get):
      |      Interpolate a source object $someObj, followed by one or more of:
      |        .someName   - Named case class-like field
      |        .$someOptic - Interpolated Monocle optic (Fold, Getter, Setter, Traversal, Optional, Prism, Lens or Iso)
      |        [0]         - Integer index, using monocle.function.Index[Int, _]
      |        [$foo]      - Interpolated index value, using implicit monocle.function.Index[Foo, _]
      |        *           - Traverses over each value, using implicit monocle.function.Each
      |        ?           - Selects a value that might not exist, using implicit monocle.function.Possible
      |
      |    Example:
      |      get"$myBakery.cakes*.toppings[0].cherry"
      |      // returns List[Cherry]
    """.stripMargin

  def usageSet =
    """
      |    Goggles usage (set):
      |      Interpolate a source object $someObj, followed by one or more of:
      |        .someName   - Named case class-like field
      |        .$someOptic - Interpolated Monocle optic (Fold, Getter, Setter, Traversal, Optional, Prism, Lens or Iso)
      |        [0]         - Integer index, using monocle.function.Index[Int, _]
      |        [$foo]      - Interpolated index value, using implicit monocle.function.Index[Foo, _]
      |        *           - Traverses over each value, using implicit monocle.function.Each
      |        ?           - Selects a value that might not exist, using implicit monocle.function.Possible
      |
      |      Followed by an operator:
      |        set"..." ~= modifyFunction
      |        set"..." := newValue
      |
      |    Example:
      |      set"$myBakery.cakes*.topping.color" := Color.Red
      |      // returns Bakery (now with red cake toppings)
    """.stripMargin

  def usageLens =
    """
      |    Goggles usage (lens):
      |      Interpolate an existing Monocle optic, followed by one or more of:
      |        .someName   - Named case class-like field
      |        .$someOptic - Interpolated Monocle optic (Fold, Getter, Setter, Traversal, Optional, Prism, Lens or Iso)
      |        [0]         - Integer index, using monocle.function.Index[Int, _]
      |        [$foo]      - Interpolated index value, using implicit monocle.function.Index[Foo, _]
      |        *           - Traverses over each value, using implicit monocle.function.Each
      |        ?           - Selects a value that might not exist, using implicit monocle.function.Possible
      |
      |    Example:
      |      lens"$restaurantMenuLens.mains*.$veganPrism"
      |      // returns monocle.Traversal[Restaurant, Meal]
    """.stripMargin
}
