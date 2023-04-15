package p752.tiles

import p752.Tiles.*
import p752.tiles.HorizontalList.Spacing.{FillWidth, Separator}
import p752.{KeyEvent, Prop, Tile}
import GenericList.{State, ItemSelected, Style}

import HorizontalList.Spacing

object HorizontalList:
  sealed trait Spacing
  object Spacing:
    final case class FillWidth(w: Int) extends Spacing
    final case class Separator(s: String) extends Spacing

class HorizontalList[T](
    renderItem: T => String,
    spacing: Spacing = Spacing.Separator("  "),
    style: Style = Style()
) extends Tile[KeyEvent, State[T], Option[ItemSelected[T]]]:

  override def render(state: State[T]): String = {
    val elements = state.items.zipWithIndex.map { case (item, i) =>
      val text = renderItem(item)
      if i == state.index
      then style.selected.render(text)
      else style.default.render(text)
    }
    spacing match
      case FillWidth(w) =>
        val textLength = elements.map(_.pureSize).sum
        val sepLen = (w - textLength) / (state.items.length + 1)
        val spare = w - (textLength + (state.items.length + 1) * sepLen)
        val separator = " ".times(sepLen)
        " ".times(spare) + separator + elements.mkString(separator) + separator

      case Separator(s) => s + elements.mkString(s) + s
  }

  override def update(
      event: KeyEvent,
      state: State[T]
  ): (State[T], Option[ItemSelected[T]]) =
    import state.{index, copy}
    event match
      case KeyEvent.Key('h') | KeyEvent.Special.Left =>
        state.dec(state.items.length) -> None
      case KeyEvent.Key('l') | KeyEvent.Special.Right | KeyEvent.Special.Tab =>
        state.inc(state.items.length) -> None
      case KeyEvent.Special.Enter =>
        state -> Some(ItemSelected(state.items(state.index)))
      case _ =>
        state -> None

end HorizontalList
