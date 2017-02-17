package goggles.macros.errors


import goggles.macros.interpret.{OpticInfo, DslMode}
import DslMode.{Lens, Set, Get}
import goggles.macros._

object TypeTableErrors {
  type Type = scala.reflect.api.Universe#Type

  def table(mode: DslMode, typedError: MacroUserError[Type], typedInfos: List[OpticInfo[Type]]): Table = {
    val infos: Vector[OpticInfo[String]] = typedInfos.map(_.map(typeString)).toVector
    val errorInfo = getErrorInfo(typedError.map(typeString))
    val sectionsColumn = Column("Sections", infos.map(_.label) ++ errorInfo.map(_.label))

    def lensTable: Table = {
      val col2a = Column("Types", infos.map(_.sourceType) ++ errorInfo.map(_.sourceType))
      val col2b = Column("", infos.map(_.targetType) ++ errorInfo.map(_.targetType))

      val typesColumn = col2a.merge(col2b, "⇒")
      val opticsColumn = Column("Optics", infos.map(opticString) ++ errorInfo.map(_.opticType))

      Table(sectionsColumn, typesColumn, opticsColumn)
    }

    def appliedTable: Table = {
      infos match {
        case Vector() => Table.empty
        case sourceInfo +: opticInfos =>
          val col2a = Column("", opticInfos.map(_.sourceType) ++ errorInfo.map(_.sourceType))
          val col2b = Column("", opticInfos.map(_.targetType) ++ errorInfo.map(_.targetType))
          val col2 = col2a.merge(col2b, "⇒")

          val typesColumn = Column("Types", sourceInfo.targetType +: col2.rawContent)
          val opticsColumn = Column("Optics", ("" +: opticInfos.map(opticString)) ++ errorInfo.map(_.opticType))

          Table(sectionsColumn, typesColumn, opticsColumn)
      }
    }

    mode match {
      case Get | Set => appliedTable
      case Lens => lensTable
    }
  }

  case class ErrorInfo(label: String, sourceType: String, targetType: String, opticType: String)

  private def getErrorInfo(e: MacroUserError[String]): Option[ErrorInfo] = e match {
    case GetterOpticRequired(_) => None
    case SetterOpticRequired(_) => None
    case NameNotFound(name, sourceType) => Some(ErrorInfo(name, sourceType, "???", ""))
    case NameNotAMethod(name, sourceType) => Some(ErrorInfo(name, sourceType, "???", ""))
    case NameHasArguments(name, sourceType) => Some(ErrorInfo(name, sourceType, "???", ""))
    case NameHasMultiParamLists(name, sourceType) => Some(ErrorInfo(name, sourceType, "???", ""))
    case InterpNotAnOptic(name, _) => Some(ErrorInfo(name, "", "", ""))
    case WrongKindOfOptic(name, sourceType, targetType, _, to) => Some(ErrorInfo(name, sourceType, targetType, to.monoTypeName))
    case TypesDontMatch(name, sourceType, targetType, _, _) => None
    case ImplicitEachNotFound(name, sourceType) => Some(ErrorInfo(name, sourceType, "???", ""))
    case ImplicitPossibleNotFound(name, sourceType) => Some(ErrorInfo(name, sourceType, "???", ""))
    case ImplicitIndexNotFound(name, sourceType, _) => Some(ErrorInfo(name, sourceType, "???", ""))
    case CopyMethodNotFound(name, sourceType) => Some(ErrorInfo(name, sourceType, "???", ""))
    case CopyMethodNotAMethod(name, sourceType) => Some(ErrorInfo(name, sourceType, "???", ""))
    case CopyMethodHasMultiParamLists(name, sourceType) => Some(ErrorInfo(name, sourceType, "???", ""))
    case CopyMethodHasNoArguments(name, sourceType) => Some(ErrorInfo(name, sourceType, "???", ""))
    case CopyMethodLacksNamedArgument(name, sourceType) => Some(ErrorInfo(name, sourceType, "???", ""))
    case CopyMethodLacksParameterDefaults(name, sourceType, _) => Some(ErrorInfo(name, sourceType, "???", ""))
  }

  private def typeString(t: Type): String = {
    def clarifyFunctionArrows(str: String) =
      if (str.contains("=>")) s"(${str.replace("=>", "⇒")})"
      else str

    val args = t.typeArgs
    val argsString = if (args.nonEmpty) args.map(typeString).mkString("[", ",", "]")
                     else ""

    val rawTypeString = s"${t.typeSymbol.name}$argsString"
    clarifyFunctionArrows(rawTypeString.trim)
  }

  private def opticString[A](info: OpticInfo[A]): String = {
    val o1 = info.opticType
    val o2 = info.compositeOpticType
    if (o1 == o2) o1.monoTypeName
    else s"${o1.monoTypeName}, returning ${o2.monoTypeName}"
  }

}
