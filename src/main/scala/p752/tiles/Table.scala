package p752.tiles

import p752.{Event, Tile}
import p752.Style
import p752.Align
import p752.Tiles._
import p752.Tiles

case class Table[T](
    items: List[T],
    headers: List[String],
    show: T => List[String],
    align: Align.Horizontal = Align.Horizontal.Center,
    index: Int = 0,
    headerStyle: Style = Table.headerStyle,
    defaultStyle: Style = Table.itemsStyle,
    selectedStyle: Style = Table.selectedStyle
) extends Tile {
  private val sep = "  "
  override val render: String = {
    val data = items.map(show)
    val lens = data.foldLeft(headers.map(_.pureSize)) { (acc, curr) =>
      acc.zip(curr).map { (len, item) =>
        val l = item.pureSize
        if len > l then len else l
      }
    }

    val renderedHeaders = headers
      .zip(lens)
      .map { (header, len) =>
        header.horizontalFit(len, align)
      }
      .mkString(sep)
      .render(headerStyle)

    val breakerLen =
      lens.sum + ((headers.length - 1) * sep.length())
    val breaker = "-".times(breakerLen)
    val renderedData = data.zipWithIndex.map { (line, ind) =>
      val style = if ind == index then selectedStyle else defaultStyle
      line
        .zip(lens)
        .map((item, len) => item.horizontalFit(len, align))
        .mkString(sep)
        .render(style)
    }

    (renderedHeaders :: breaker :: renderedData).mkString("\n")
  }
  override def update(e: Event): Table[T] = {
    this
  }
}

object Table:
  val headerStyle = Style(foreground = 231, background = 54, bold = true)
  val itemsStyle = Style()
  val selectedStyle = Style(foreground = 231, background = 17, italic = true)
