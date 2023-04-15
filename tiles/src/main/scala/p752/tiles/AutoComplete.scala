package p752.tiles

import p752.Prop
import p752.Tile
import p752.KeyEvent
import p752.Tiles

object AutoComplete:
  final case class State[T](
      items: List[T],
      inputState: Input.State,
      listState: GenericList.State[T]
  )
  final case class Style(
      inputStyle: Input.Style = Input.defaultStyle,
      listStyle: GenericList.Style = GenericList.defaultStyle
  )

  val defaultStyle = Style()
  def defaultState[T](items: List[T]): State[T] =
    val inputState = Input.State()
    val listState = GenericList.State(items)
    State(items, inputState, listState)

  sealed trait Event[+T]
  object Event:
    case object Pass extends Event[Nothing]
    case class ItemMatched[+T](t: T) extends Event[T]
    case class ItemSelected[+T](t: T) extends Event[T]

import AutoComplete.{State, Event, Style}

final case class AutoComplete[T](
    renderItem: T => String,
    placeHolder: String = "",
    style: Style = AutoComplete.defaultStyle
) extends Tile[KeyEvent, State[T], Event[T]]:
  val input: Input = Input(placeHolder, style.inputStyle)
  val itemList: VerticalList[T] = VerticalList(renderItem, style.listStyle)

  override def render(state: State[T]): String = Tiles.renderVertical(
    input.render(state.inputState),
    " ",
    itemList.render(state.listState)
  )

  override def update(
      event: KeyEvent,
      state: State[T]
  ): (State[T], Event[T]) =
    event match
      case KeyEvent.Special.Enter =>
        state.listState.selected match
          case Some(t) => state -> Event.ItemSelected(t)
          case None    => state -> Event.Pass

      case KeyEvent.Special.Down | KeyEvent.Special.Up =>
        state.copy(listState =
          itemList.update(event, state.listState)._1
        ) -> Event.Pass

      case KeyEvent.Special.Tab =>
        val newInputState = state.listState.selected match {
          case None =>
            state.inputState
          case Some(item) =>
            val newText = renderItem(item)
            state.inputState.copy(newText, cursor = newText.length)
        }
        val newItems =
          state.items
            .filter(i =>
              renderItem(i)
                .toLowerCase()
                .startsWith(newInputState.text.toLowerCase())
            )
        val newListState = GenericList.State(newItems, 0)
        val msg = newListState.selected match
          case None        => Event.Pass
          case Some(value) => Event.ItemMatched(value)
        State(state.items, newInputState, newListState) -> msg

      case event =>
        val updatedInputState = input.update(event, state.inputState)._1
        val lower = updatedInputState.text.toLowerCase()
        val newItems =
          state.items
            .filter(i => renderItem(i).toLowerCase().startsWith(lower))
        state.copy(listState = state.listState.copy(newItems, 0), inputState = updatedInputState) -> Event.Pass

  end update

end AutoComplete
