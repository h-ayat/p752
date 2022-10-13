package p752.tiles

import p752.Style
import p752.Tile
import p752.Event

final case class VerticalList[T](
    items: List[T],
    renderItem: T => String,
    defaultStyle: Style = Style.empty,
    selectedStyle: Style = Style.empty,
    index: Int = 0,
    finished: Boolean = false
) extends Tile {

  val selected = items(index)

  override val render: String = {
    items
      .map(renderItem)
      .zipWithIndex
      .map { case (s, ind) =>
        val style = if ind == index then selectedStyle else defaultStyle
        style.render(s)
      }
      .mkString("\n")
  }
  override def update(event: Event): Tile = event match {
    case _ if finished => this
    case Event.Key('j') | Event.Special.Down =>
      val ind =
        if index >= items.length - 1
        then items.length - 1
        else index + 1
      this.copy(index = ind)

    case Event.Key('k') | Event.Special.Up =>
      val ind = if index > 0 then index - 1 else 0
      this.copy(index = ind)

    case Event.Special.Enter =>
      this.copy(finished = true)
    case _ =>
      this
  }
}
