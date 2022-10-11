package p752.tiles

import p752.Tile
import p752.Event
import p752.Style
import p752.tiles.HorizontalList.Spacing.FillWidth
import p752.tiles.HorizontalList.Spacing.Seperator
import p752.StringUtils._

case class HorizontalList[T](
    items: List[T],
    renderItem: T => String,
    spacing: HorizontalList.Spacing,
    defaultStyle: Style = Style.empty,
    selectedStyle: Style = Style.empty,
    index: Int = 0,
    finished: Boolean = false
) extends Tile {

  val selected = items(index)

  val render: String = {
    val elements = items.zipWithIndex.map { case (item, i) =>
      val text = renderItem(item)
      if i == index then selectedStyle.render(text)
      else defaultStyle.render(text)
    }
    spacing match
      case FillWidth(w) =>
        val textLength = elements.map(_.pureSize).sum
        println(textLength)
        val sepLen = (w - textLength) / (items.length + 1)
        println(sepLen)
        println(w)
        val spare = w - (textLength + (items.length + 1) * sepLen)
        val seperator = " ".times(sepLen)
        " ".times(spare) + seperator + elements.mkString(seperator) + seperator
      case Seperator(s) => s + elements.mkString(s) + s
  }

  val update: PartialFunction[Event, HorizontalList[T]] = {
    case Event.Special.Left if index > 0 =>
      copy(index = index - 1)
    case Event.Special.Right if index < items.length - 1 =>
      copy(index = index + 1)

    case Event.Special.Tab =>
      var ind = index + 1
      if ind >= items.length then ind = 0
      copy(index = ind)

    case Event.Special.Enter =>
      copy(finished = true)

  }

}

object HorizontalList:
  sealed trait Spacing
  object Spacing:
    final case class FillWidth(w: Int) extends Spacing
    final case class Seperator(s: String) extends Spacing
