package goggles.macros.errors

private[goggles] object Table {
  def apply(cols: Column*): Table = Table(cols.toList)
  def empty = Table(Nil)
}

private[goggles] case class Table(columns: List[Column]) {
  def render: String = {
    if (columns.isEmpty) ""
    else {
      val headings = columns.map(_.formattedHeading).mkString("│")
      val separator = columns.map(_.separator).mkString("┼")
      val maxRows = columns.map(_.numRows).max

      val lines: List[String] =
        0.until(maxRows).toList.map { row =>
          columns.map(_.getFormattedCell(row)).mkString("│")
        }

      val body = lines.mkString("\n")

      s"\n$headings\n$separator\n$body\n"
    }
  }
}
