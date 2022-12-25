package p752.tiles

import p752.Tiles.*
import p752.*

case class Table[T](
    value: T,
    headers: List[String],
    show: T => List[List[String]],
    title: Option[String],
    align: Align.Horizontal = Align.Horizontal.Center,
    index: Int = 0,
    headerStyle: Style = Table.headerStyle,
    defaultStyle: Style = Table.itemsStyle,
    selectedStyle: Style = Table.selectedStyle
) extends Tile[Any] {
  private val data = show(value)
  private val sep = "  "
  override val render: String = {
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
  override def update(event: Either[Event, Any]): Table[T] = event match {
    case Right(_) => this
    case Left(e) =>
      e match {
        case Event.Key('j') | Event.Special.Down =>
          val ind = (index + 1) % data.length
          this.copy(index = ind)

        case Event.Key('k') | Event.Special.Up =>
          val ind = if index <= 0 then data.length - 1 else index - 1
          this.copy(index = ind)
        case _ =>
          this

      }
  }
}

object Table:
  val headerStyle: Style = Style(foreground = 231, background = 54, bold = true)
  val itemsStyle: Style = Style()
  val selectedStyle: Style = Style(foreground = 231, background = 17, italic = true)
