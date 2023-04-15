package p752.tiles

import p752.Padding
import p752.Border
import p752.Prop
import p752.KeyEvent
import p752.Tiles
import p752.Align
import p752.Tile

import GenericList.{State, ItemSelected}
import Prompt.{Style}

object Prompt:
  private val pink = 161
  private val white = 231

  val defaultPadding = Padding(1, 1, 1, 1)
  val defaultBorder = Border(Prop.empty.copy(foreground = pink))
  val defaultStyle = Style()

  final case class Style(
      border: Border = Prompt.defaultBorder,
      padding: Padding = Prompt.defaultPadding
  )

class Prompt[T](
    message: String,
    show: T => String,
    style: Style = Prompt.defaultStyle
) extends Tile[KeyEvent, State[T], Option[ItemSelected[T]]]:
  private val messageParts = message.split("\n").toList
  private val hList =
    HorizontalList[T](show, HorizontalList.Spacing.Separator("  "))

  override def render(state: State[T]): String =
    val parts = messageParts :+ "" :+ hList.render(state)
    val content = Tiles.renderHorizontal(Align.Horizontal.Center, parts: _*)
    style.border.render(style.padding.render(content))

  override def update(
      event: KeyEvent,
      state: State[T]
  ): (State[T], Option[ItemSelected[T]]) =
    hList.update(event, state)
