package goggles.macros.errors


import goggles.macros.interpret.{OpticInfo, DslMode}
import DslMode.{Lens, Set, Get}

private[goggles] object TypeTableErrors {
  type Type = scala.reflect.api.Universe#Type

  def table(mode: DslMode, typedError: UserError[Type], typedInfos: List[OpticInfo[Type]]): Table = {
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

    if (infos.length < 2) Table.empty
    else mode match {
      case Get | Set => appliedTable
      case Lens => lensTable
    }
  }

  case class ErrorInfo(label: String, sourceType: String, targetType: String, opticType: String)

  private def getErrorInfo(e: UserError[String]): Option[ErrorInfo] = e match {
    case UserError.GetterOpticRequired(_) => None
    case UserError.SetterOpticRequired(_) => None
    case UserError.NameNotFound(name, sourceType) => Some(ErrorInfo(name, sourceType, "???", ""))
    case UserError.NameNotAMethod(name, sourceType) => Some(ErrorInfo(name, sourceType, "???", ""))
    case UserError.NameHasArguments(name, sourceType) => Some(ErrorInfo(name, sourceType, "???", ""))
    case UserError.NameHasMultiParamLists(name, sourceType) => Some(ErrorInfo(name, sourceType, "???", ""))
    case UserError.InterpNotAnOptic(name, _) => Some(ErrorInfo(name, "", "", ""))
    case UserError.WrongKindOfOptic(name, sourceType, targetType, _, to) => Some(ErrorInfo(name, sourceType, targetType, to.monoTypeName))
    case UserError.TypesDontMatch(name, sourceType, targetType, _, _) => None
    case UserError.ImplicitEachNotFound(name, sourceType) => Some(ErrorInfo(name, sourceType, "???", ""))
    case UserError.ImplicitPossibleNotFound(name, sourceType) => Some(ErrorInfo(name, sourceType, "???", ""))
    case UserError.ImplicitIndexNotFound(name, sourceType, _) => Some(ErrorInfo(name, sourceType, "???", ""))
    case UserError.CopyMethodNotFound(name, sourceType) => Some(ErrorInfo(name, sourceType, "???", ""))
    case UserError.CopyMethodNotAMethod(name, sourceType) => Some(ErrorInfo(name, sourceType, "???", ""))
    case UserError.CopyMethodHasMultiParamLists(name, sourceType) => Some(ErrorInfo(name, sourceType, "???", ""))
    case UserError.CopyMethodHasNoArguments(name, sourceType) => Some(ErrorInfo(name, sourceType, "???", ""))
    case UserError.CopyMethodLacksNamedArgument(name, sourceType) => Some(ErrorInfo(name, sourceType, "???", ""))
    case UserError.CopyMethodLacksParameterDefaults(name, sourceType, _) => Some(ErrorInfo(name, sourceType, "???", ""))
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
