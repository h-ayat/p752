package p752.tiles

import p752.{KeyEvent, Style, Tile}

final case class VerticalList[T](
    items: List[T],
    renderItem: T => String,
    defaultStyle: Style = Style.empty,
    selectedStyle: Style = Style.empty,
    selectedIndex: Int = 0,
    finished: Boolean = false,
    offset: Int = 0,
    limit: Int = 10
) extends Tile[KeyEvent] {

  val selected: Option[T] =
    if items.size > selectedIndex then Some(items(selectedIndex)) else None

  override val render: String = {
    items.zipWithIndex
      .drop(offset)
      .take(limit)
      .map { (s, ind) =>
        val style = ind match
          case i if i == selectedIndex => selectedStyle
          case _                       => defaultStyle

        style.render(renderItem(s))
      }
      .mkString("\n")
  }

  private def changeIndex(diff: Int): VerticalList[T] =
    val newIndex: Int = {
      val ind = selectedIndex + diff
      if ind < 0 then 0
      else if ind > (items.length - 1) then items.length - 1
      else ind
    }

    val newOffset =
      if newIndex < offset then newIndex
      else if newIndex >= offset + limit then newIndex + 1 - limit
      else offset
    this.copy(selectedIndex = newIndex, offset = newOffset)

  override def update(event: KeyEvent): VerticalList[T] =
    event match
      case _ if finished => this
      case KeyEvent.Key('j') | KeyEvent.Special.Down =>
        changeIndex(1)

      case KeyEvent.Key('k') | KeyEvent.Special.Up =>
        changeIndex(-1)

      case KeyEvent.Special.Enter =>
        this.copy(finished = true)
      case _ =>
        this

}
