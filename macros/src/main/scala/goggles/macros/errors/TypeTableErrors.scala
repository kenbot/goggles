package goggles.macros.errors

import goggles.macros.{OpticType, DslMode, MacroUserError}

import scala.reflect.api.Universe

object TypeTableErrors {

  type OpticInfo = goggles.macros.OpticInfo[Universe#Type]

  private def typeString(t: Universe#Type): String = {
    def removeLeadingArrow(str: String) =
      if (str.substring(0,3) == "=> ") str.substring(3)
      else str

    def clarifyFunctionArrows(str: String) =
      if (str.contains("=>")) s"(${str.replace("=>", "⇒")})"
      else str

    clarifyFunctionArrows(removeLeadingArrow(t.toString).trim)
  }

  private def opticString(info: OpticInfo): String = {
    val o1 = info.opticType
    val o2 = info.compositeOpticType
    if (o1 == o2) o1.monoTypeName
    else s"${o1.monoTypeName}, returning ${o2.monoTypeName}"
  }

  case class Col(heading: String, rawContent: List[String], maxWidth: Option[Int] = None) {
    def margin = 1

    val contentWidth = (heading :: rawContent).map(c => trunc(c.trim).length).max
    val columnWidth = contentWidth + margin * 2

    val separator = checkWidth("─" * columnWidth)
    val formattedHeading = format(heading)
    val formattedContent = rawContent.map(format)

    def getFormattedCell(row: Int): String =
      formattedContent.lift(row).getOrElse(format(""))

    def numRows: Int =
      rawContent.length

    def merge(otherCol: Col, internalSeparator: String): Col = {
      val both = formattedContent.zip(otherCol.formattedContent)
      val newContent = both.map { case (a,b) => s"$a $internalSeparator $b" }
      val newMaxWidth = (maxWidth, otherCol.maxWidth) match {
        case (Some(w1), Some(w2)) => Some(w1 + w2)
        case (Some(w), None) => Some(w)
        case (None, Some(w)) => Some(w)
        case (None, None) => None
      }
      Col(heading, newContent, newMaxWidth)
    }

    private def trunc(str: String) = maxWidth match {
      case Some(max) if str.length > max => str.substring(0, max-3) + "..."
      case _ => str
    }

    private def format(rawCell: String): String = {
      val cell = trunc(rawCell.trim)
      checkWidth(" " + cell + (" " * (contentWidth - cell.length + 1)))
    }

    private def checkWidth(str: String): String = {
      if (str.length == columnWidth) str
      else throw new IllegalStateException(s"Column '$heading' expected width $columnWidth, but was ${str.length}: |$str|")
    }
  }

  def message(mode: DslMode, infos: List[OpticInfo]): String = {

    def table(cols: Col*): String = {
      val headings = cols.map(_.formattedHeading).mkString("│")
      val separator = cols.map(_.separator).mkString("┼")
      val maxRows = cols.map(_.numRows).max

      val lines: List[String] =
        0.until(maxRows).toList.map { row =>
          cols.map(_.getFormattedCell(row)).mkString("│")
        }

      val body = lines.mkString("\n")

      s"\n$headings\n$separator\n$body\n"
    }

    val col1 = Col("Sections", infos.map(_.label), Some(20))
    val col2 = Col("Types", infos.map(i => typeString(i.sourceType)))
    val col3 = Col("", infos.map(i => typeString(i.targetType)))
    val col4 = Col("Optics", infos.map(opticString))

    table(col1, col2.merge(col3, "⇒"), col4)

/*

    val col1MaxWidth = 20
    val col1Width = Math.min(infos.map(i => i.label.length).max, col1MaxWidth) + 1
    val col2aWidth = infos.map(i => typeString(i.sourceType).length).max + 1
    val col2bWidth = infos.map(i => typeString(i.targetType).length).max + 4
    val col2Width = col2aWidth + col2bWidth + 2
    val col3Width = infos.map(i => opticString(i).length).max + 1

    val col2Start = col1Width
    val col3Start = col2Start + col2Width
    val col3End = col3Start + col3Width + 1

    def trunc(str: String, maxLength: Int) = str
      // if (str.length <= maxLength) str
      // else str.substring(0, maxLength-3) + "..."

    def padded(str: String, toLength: Int) = {
      str + (" " * (toLength - str.length))
    }

    def col1(str: String) = padded(trunc(str, col1MaxWidth), col1Width)
    def col2(src: String, tgt: String) =
      padded(src, col2aWidth) + padded(tgt, col2bWidth)

    def objectLine(info: OpticInfo) =
      col1(info.label) + col2(s"│ ${typeToString(info.targetType)}", "") + "│\n"

    def opticLine(info: OpticInfo) =
      col1(info.label) + col2(s"│ ${typeToString(info.sourceType)}", s" ⇒ ${typeToString(info.targetType)}") + s"│ ${opticString(info)}\n"

    val heading = col1("Section") + col2("│ Types", "") + "│ Optics\n"
    val separator = "─" * col1Width + "┼" + ("─" * col2Width) + "┼" + ("─" * col3Width) + "\n"

    val lines = infos match {
      case first :: rest => objectLine(first) :: rest.map(opticLine)
      case Nil => Nil
    }

    "\n" + heading + separator + lines.foldLeft("")(_ + _)

    */
  }

}
