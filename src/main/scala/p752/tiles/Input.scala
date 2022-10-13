package p752.tiles

import p752.Style
import p752.Tile
import p752.Event

final case class Input(
    text: String,
    placeHolder: String = "",
    style: Style = Style.empty,
    placeHolderStyle: Style = Style.empty
) extends Tile {

  override val render: String =
    if text.isEmpty() then placeHolderStyle.render(placeHolder)
    else style.render(text)

  override def update(event: Event): Input = event match {
    case Event.Key(ch) =>
      this.copy(text = text + ch)
    case _ =>
      this
  }
}
