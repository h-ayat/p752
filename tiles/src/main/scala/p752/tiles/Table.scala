package p752.tiles

import p752.Tiles.*
import p752.*
import p752.tiles.Table.State
import p752.tiles.Table.Msg
import scala.scalanative.unsafe.extern

object Table:
  case class State[T](raw: List[T], x: Int = 0, y: Int = 0)
  sealed trait Msg
  object Msg:
    case class SelectionChanged(x: Int, y: Int) extends Msg

  protected val defaultTitleStyle: Prop =
    Prop(foreground = 40, background = 16, bold = true)
  protected val defaultHeaderStyle: Prop =
    Prop(foreground = 255, background = 22, bold = true)
  protected val defaultItemsStyle: Prop = Prop.empty
  protected val defaultSelectedStyle: Prop =
    Prop(foreground = 231, background = 17, italic = true)

case class Table[T](
    headers: List[String],
    show: T => List[String],
    title: Option[String],
    align: Align.Horizontal = Align.Horizontal.Center,
    titleStyle: Prop = Table.defaultTitleStyle,
    headerStyle: Prop = Table.defaultHeaderStyle,
    defaultStyle: Prop = Table.defaultItemsStyle,
    xSelectedStyle: Prop = Table.defaultSelectedStyle,
    ySelectedStyle: Prop = Table.defaultSelectedStyle
) extends Tile[KeyEvent, Table.State[T], Option[Table.Msg]]:
  import Table.{State, Msg}
  private val sep = "  "

  override def render(state: State[T]): String =
    val data = state.raw.map(show)
    val lens = data.foldLeft(headers.map(_.pureSize)) { (acc, curr) =>
      acc.zip(curr).map { (len, item) =>
        val l = item.pureSize
        if len > l then len else l
      }
    }

    val renderedHeaders = {
      val columns = headers
        .zip(lens)
        .map { (header, len) =>
          header.horizontalFit(len, align)
        }
        .mkString(sep)
        .render(headerStyle)
      val t =
        titleStyle.render(
          title
            .getOrElse("")
            .horizontalFit(columns.pureSize, Align.Horizontal.Center)
        )
      Tiles.renderVertical(t, columns)

    }

    val breakerLen =
      lens.sum + ((headers.length - 1) * sep.length())
    val breaker = "-".times(breakerLen)
    val renderedData = data.zipWithIndex.map { (line, ind) =>
      val yStyle = if ind == state.y then ySelectedStyle else defaultStyle
      line
        .zip(lens)
        .map((item, len) => item.horizontalFit(len, align))
        .zipWithIndex
        .map {
          case (value, x) if x == state.x =>
            value.render(xSelectedStyle)
          case (value, _) =>
            value.render(yStyle)
        }
        .mkString(yStyle.render(sep))
    }

    (renderedHeaders :: breaker :: renderedData).mkString("\n")
  end render

  override def update(
      event: KeyEvent,
      state: State[T]
  ): (State[T], Option[Msg]) =
    val newState = event match {
      case KeyEvent.Key('h') | KeyEvent.Special.Left =>
        val ind = if state.x <= 0 then headers.length - 1 else state.x - 1
        state.copy(x = ind)

      case KeyEvent.Key('l') | KeyEvent.Special.Right =>
        val ind = (state.x + 1) % headers.length
        state.copy(x = ind)

      case KeyEvent.Key('j') | KeyEvent.Special.Down =>
        val ind = (state.y + 1) % state.raw.length
        state.copy(y = ind)

      case KeyEvent.Key('k') | KeyEvent.Special.Up =>
        val ind = if state.y <= 0 then state.raw.length - 1 else state.y - 1
        state.copy(y = ind)
      case _ =>
        state
    }
    if newState == state then state -> None
    else newState -> Some(Msg.SelectionChanged(newState.x, newState.y))
  end update

end Table
