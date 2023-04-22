package p752.tiles

import p752.Tile
import p752.Padding
import p752.Border
import p752.Style
import p752.KeyEvent
import p752.Tiles
import p752.Align

object Prompt:
  private val pink = 161
  private val white = 231
  private val selectedStyle =
    Style.apply(foreground = white, background = pink, bold = true)

  def apply[T](
      message: String,
      options: List[T],
      show: T => String
  ): Prompt[T] = {
    val list = new HorizontalList[T](
      options,
      show,
      HorizontalList.Spacing.Separator("  "),
      selectedStyle = selectedStyle,
      index = 0
    )

    Prompt(
      message = message,
      list = list,
      result = None
    )
  }

case class Prompt[T](
    message: String,
    list: HorizontalList[T],
    result: Option[T]
) extends Tile[KeyEvent] {

  private val messageParts = message.split("\n").toList
  private val padding = Padding(1, 1, 1, 1)
  private val border = Border(Style.empty.copy(foreground = Prompt.pink))

  override def update(event: KeyEvent): Prompt[T] =
    (result, event) match
      case None -> KeyEvent.Special.Enter =>
        this.copy(result = Some(list.selected))
      case None -> _ =>
        this.copy(list = list.update(event))
      case Some(value) -> _ =>
        this

  override val render: String =
    val parts = messageParts :+ "" :+ list.render
    val content = Tiles.renderHorizontal(Align.Horizontal.Center, parts: _*)
    border.render(padding.render(content))

}
