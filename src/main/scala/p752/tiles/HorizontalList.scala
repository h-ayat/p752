package p752.tiles

import p752.Tiles.*
import p752.tiles.HorizontalList.Spacing.{FillWidth, Separator}
import p752.{Event, Style, Tile}

case class HorizontalList[T](
    items: List[T],
    renderItem: T => String,
    spacing: HorizontalList.Spacing,
    defaultStyle: Style = Style.empty,
    selectedStyle: Style = Style.empty,
    index: Int = 0,
    finished: Boolean = false
) extends Tile[Any] {

  val selected: T = items(index)

  val render: String = {
    val elements = items.zipWithIndex.map { case (item, i) =>
      val text = renderItem(item)
      if i == index then selectedStyle.render(text)
      else defaultStyle.render(text)
    }
    spacing match
      case FillWidth(w) =>
        val textLength = elements.map(_.pureSize).sum
        val sepLen = (w - textLength) / (items.length + 1)
        val spare = w - (textLength + (items.length + 1) * sepLen)
        val separator = " ".times(sepLen)
        " ".times(spare) + separator + elements.mkString(separator) + separator
      case Separator(s) => s + elements.mkString(s) + s
  }

  def update(event: Either[Event, Any]): HorizontalList[T] = event match {
    case Right(_) => this
    case Left(e) =>
      e match {
        case _ if finished =>
          this
        case Event.Key('h') | Event.Special.Left if index > 0 =>
          copy(index = index - 1)
        case Event.Key('l') | Event.Special.Right if index < items.length - 1 =>
          copy(index = index + 1)

        case Event.Special.Tab =>
          var ind = index + 1
          if ind >= items.length then ind = 0
          copy(index = ind)

        case Event.Special.Enter =>
          copy(finished = true)

        case _ =>
          this
      }

  }

}

object HorizontalList:
  sealed trait Spacing
  object Spacing:
    final case class FillWidth(w: Int) extends Spacing
    final case class Separator(s: String) extends Spacing
