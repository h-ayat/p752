package p752.tiles

import p752.Tiles.*
import p752.*

case class Table[T](
    headers: List[String],
    raw: List[T],
    show: T => List[String],
    title: Option[String],
    align: Align.Horizontal = Align.Horizontal.Center,
    x: Int = 0,
    y: Int = 0,
    headerStyle: Style = Table.headerStyle,
    defaultStyle: Style = Table.itemsStyle,
    xSelectedStyle: Style = Table.selectedStyle,
    ySelectedStyle: Style = Table.selectedStyle
) extends Tile[Any] {

  private val data = raw.map(show)
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
      val yStyle = if ind == y then ySelectedStyle else defaultStyle
      line
        .zip(lens)
        .map((item, len) => item.horizontalFit(len, align))
        .zipWithIndex
        .map {
          case (value, x) if x == this.x =>
            value.render(xSelectedStyle)
          case (value, _) =>
            value.render(yStyle)
        }
        .mkString(yStyle.render(sep))
    }

    (renderedHeaders :: breaker :: renderedData).mkString("\n")
  }
  override def update(event: Either[Event, Any]): Table[T] = event match {
    case Right(_) => this
    case Left(e) =>
      e match {

        case Event.Key('h') | Event.Special.Left =>
          val ind = if x <= 0 then headers.length - 1 else x - 1
          this.copy(x = ind)

        case Event.Key('l') | Event.Special.Right =>
          val ind = (x + 1) % headers.length
          this.copy(x = ind)

        case Event.Key('j') | Event.Special.Down =>
          val ind = (y + 1) % data.length
          this.copy(y = ind)

        case Event.Key('k') | Event.Special.Up =>
          val ind = if y <= 0 then data.length - 1 else y - 1
          this.copy(y = ind)
        case _ =>
          this

      }
  }
}

object Table:
  val headerStyle: Style = Style(foreground = 231, background = 54, bold = true)
  val itemsStyle: Style = Style()
  val selectedStyle: Style =
    Style(foreground = 231, background = 17, italic = true)
