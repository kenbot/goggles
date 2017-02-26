package goggles.macros.errors

private[goggles] case class Column(heading: String, rawContent: Vector[String], maxWidth: Option[Int] = None) {
  def margin = 1

  val contentWidth = (heading +: rawContent).map(c => trunc(c.trim).length).max
  val columnWidth = contentWidth + margin * 2

  val separator = checkWidth("─" * columnWidth)
  val formattedHeading = format(heading)
  val formattedContent = rawContent.map(format)

  def getFormattedCell(row: Int): String =
    formattedContent.lift(row).getOrElse(format(""))

  def numRows: Int =
    rawContent.length

  def merge(otherCol: Column, internalSeparator: String): Column = {
    val both = formattedContent.zip(otherCol.formattedContent)
    val newContent = both.map {
      case (a,b) =>
        s"$a $internalSeparator $b"
    }
    val newMaxWidth = (maxWidth, otherCol.maxWidth) match {
      case (Some(w1), Some(w2)) => Some(w1 + w2)
      case (Some(w), None) => Some(w)
      case (None, Some(w)) => Some(w)
      case (None, None) => None
    }
    Column(heading, newContent, newMaxWidth)
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
