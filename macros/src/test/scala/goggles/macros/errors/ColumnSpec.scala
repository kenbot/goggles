package goggles.macros.errors

import org.specs2._

import goggles.macros.Generators._

class ColumnSpec extends Specification with ScalaCheck { def is =
  s2"""
       Formatted cell by row should match the column width
       $formattedCellMatchesColumnWidth

       Heading width should match the column width
       $formattedHeadingMatchesColumnWidth

       All formatted cells should match the column width
       $formattedContentMatchesColumnWidth
    """

  def formattedCellMatchesColumnWidth = prop { (i: Int, c: Column) =>
    c.getFormattedCell(i).length === c.columnWidth
  }

  def formattedHeadingMatchesColumnWidth = prop { c: Column =>
    c.formattedHeading.length == c.columnWidth
  }

  def formattedContentMatchesColumnWidth = prop { c: Column =>
    c.formattedContent.forall(_.length == c.columnWidth)
  }
}
