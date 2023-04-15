package p752.tiles

import p752.{KeyEvent, Prop, Tile}
import GenericList.{State, ItemSelected, Style}

class VerticalList[T](
    renderItem: T => String,
    style: Style = Style(),
    offset: Int = 0,
    limit: Int = 10
) extends Tile[KeyEvent, State[T], Option[ItemSelected[T]]]:

  override def render(state: State[T]): String = 
    state.items.zipWithIndex
      .drop(offset)
      .take(limit)
      .map { (s, ind) =>
        val currentStyle = ind match
          case i if i == state.index => style.selected
          case _                     => style.default

        currentStyle.render(renderItem(s))
      }
      .mkString("\n")

  override def update(
      event: KeyEvent,
      state: State[T]
  ): (State[T], Option[ItemSelected[T]]) =
    event match 
      case KeyEvent.Key('j') | KeyEvent.Special.Down | KeyEvent.Special.Tab =>
        state.inc(state.items.length) -> None
      case KeyEvent.Key('k') | KeyEvent.Special.Up =>
        state.dec(state.items.length) -> None

      case KeyEvent.Special.Enter =>
        state -> Some(ItemSelected(state.items(state.index)))
      case _ =>
        state -> None

