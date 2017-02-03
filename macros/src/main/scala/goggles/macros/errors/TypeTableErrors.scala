package goggles.macros.errors


import scala.reflect.api.Universe

object TypeTableErrors {

  type OpticInfo = goggles.macros.OpticInfo[Universe#Type]


  def lensMessage(infos: List[OpticInfo]): String = {
    val sectionsColumn = Column("Sections", infos.map(_.label))

    val col2a = Column("Types", infos.map(i => typeString(i.sourceType)))
    val col2b = Column("", infos.map(i => typeString(i.targetType)))

    val typesColumn = col2a.merge(col2b, "⇒")
    val opticsColumn = Column("Optics", infos.map(opticString))

    Column.renderTable(sectionsColumn, typesColumn, opticsColumn)
  }

  def appliedMessage(infos: List[OpticInfo]): String = {
    val sectionsColumn = Column("Sections", infos.map(_.label))

    infos match {
      case Nil => ""
      case sourceInfo :: opticInfos =>
        val col2a = Column("", opticInfos.map(i => typeString(i.sourceType)))
        val col2b = Column("", opticInfos.map(i => typeString(i.targetType)))
        val col2 = col2a.merge(col2b, "⇒")

        val sourceInfoType = typeString(sourceInfo.targetType)

        val typesColumn = Column("Types", sourceInfoType :: col2.rawContent)
        val opticsColumn = Column("Optics", "" :: opticInfos.map(opticString))

        Column.renderTable(sectionsColumn, typesColumn, opticsColumn)
    }
  }

  private def typeString(t: Universe#Type): String = {
    def removeLeadingArrow(str: String) =
      if (str.substring(0,3) == "=> ") str.substring(3)
      else str

    def clarifyFunctionArrows(str: String) =
      if (str.contains("=>")) s"(${str.replace("=>", "⇒")})"
      else str

    val args = t.typeArgs
    val argsString = if (args.nonEmpty) args.map(typeString).mkString("[", ",", "]")
                     else ""
    val rawTypeString = s"${t.typeSymbol.name}$argsString"
    clarifyFunctionArrows(removeLeadingArrow(rawTypeString).trim)
  }

  private def opticString(info: OpticInfo): String = {
    val o1 = info.opticType
    val o2 = info.compositeOpticType
    if (o1 == o2) o1.monoTypeName
    else s"${o1.monoTypeName}, returning ${o2.monoTypeName}"
  }

}
