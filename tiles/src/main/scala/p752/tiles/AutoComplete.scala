package p752.tiles

import p752.Style
import p752.Tile
import p752.KeyEvent
import p752.Tiles

object AutoComplete {

  def apply[T](
      text: String = "",
      placeHolder: String = "",
      style: Style = Style.empty,
      placeHolderStyle: Style = Style.empty,
      cursorStyle: Style = Style(background = 236, blinking = true),
      cursor: Int = 0,
      items: List[T],
      renderItem: T => String,
      showLimit: Int = 5,
      listDefaultStyle: Style = Style.empty,
      listSelectedStyle: Style = Style(background = 198, foreground = 231)
  ): AutoComplete[T] = {
    val input =
      Input(text, placeHolder, style, placeHolderStyle, cursorStyle, cursor)
    val list =
      VerticalList(
        items,
        renderItem,
        listDefaultStyle,
        listSelectedStyle,
        limit = showLimit
      )
    AutoComplete(input, list, None, items)
  }
}

final case class AutoComplete[T](
    input: Input,
    itemList: VerticalList[T],
    result: Option[T],
    allItems: List[T]
) extends Tile[KeyEvent]:
  import itemList.renderItem
  override val render: String =
    Tiles.renderVertical(input.render, " ", itemList.render)

  override def update(event: KeyEvent): AutoComplete[T] = event match
    case _ if result.nonEmpty =>
      this

    case KeyEvent.Special.Enter =>
      this.copy(input, itemList, itemList.selected)

    case KeyEvent.Special.Down | KeyEvent.Special.Up =>
      this.copy(itemList = itemList.update(event))

    case KeyEvent.Special.Tab =>
      val newInput = itemList.selected match
        case None =>
          input
        case Some(value) =>
          val newText = renderItem(value)
          input.copy(newText, cursor = newText.length)

      val newItems =
        allItems
          .filter(i =>
            renderItem(i).toLowerCase().startsWith(newInput.text.toLowerCase())
          )
      val updatedList = itemList.copy(items = newItems, selectedIndex = 0)
      this.copy(input = newInput, itemList = updatedList)

    case _ =>
      val updatedInput = input.update(event)
      val lower = updatedInput.text.toLowerCase()
      val newItems =
        allItems
          .filter(i => renderItem(i).toLowerCase().startsWith(lower))
      val updatedList = itemList.copy(items = newItems, selectedIndex = 0)
      this.copy(input = updatedInput, itemList = updatedList)
  end update

end AutoComplete
