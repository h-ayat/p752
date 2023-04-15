package p752.tiles

import p752.Prop
import p752.KeyEvent
import p752.Tiles._
import p752.Tile

import Input.{State, Style}

object Input:
  val delimiters: Set[Char] = " -!@#&()[{}]:;',?/*\"`~$^+=<>".toSet

  final case class State(
      text: String = "",
      cursor: Int = 0
  )

  final case class Style(
      text: Prop = Prop.empty,
      placeHolder: Prop = Prop.empty,
      cursor: Prop = Prop(background = 236, blinking = true)
  )

  val defaultStyle = Style()
  val defaultState = State()

class Input(
    placeHolder: String = "",
    style: Style = Input.defaultStyle
) extends Tile[KeyEvent, State, Option[String]] {
  import Input.State

  override def render(state: State): String =
    if state.text.isEmpty then renderText(placeHolder, style.placeHolder)(state)
    else renderText(state.text, style.text)(state)

  override def update(event: KeyEvent, state: State): (State, Option[String]) =
    import state.{text, cursor}
    event match
      case KeyEvent.Key(ch) =>
        val t = text.take(cursor) + ch + text.drop(cursor)
        state.copy(text = t, cursor = cursor + 1) -> None
      case KeyEvent.Special.ETB =>
        val killStart = (1 to (cursor - 1)).reverse
          .find { i =>
            Input.delimiters.contains(text(i - 1))
          }
          .getOrElse(0)
        val finalText =
          text.substring(0, killStart) + text.substring(cursor)
        state.copy(text = finalText, cursor = killStart) -> None
      case KeyEvent.Special.Left =>
        val c = if cursor == 0 then 0 else cursor - 1
        state.copy(cursor = c) -> None
      case KeyEvent.Special.Right =>
        val c = if cursor == text.length() then cursor else cursor + 1
        state.copy(cursor = c) -> None
      case KeyEvent.Special.Backspace =>
        val t = text.take(cursor - 1) + text.drop(cursor)
        val c = if cursor == 0 then 0 else cursor - 1
        state.copy(text = t, cursor = c) -> None
      case KeyEvent.Special.Del =>
        val t = text.take(cursor) + text.drop(cursor + 1)
        state.copy(text = t) -> None
      case KeyEvent.Special.Enter =>
        state -> Some(state.text)
      case _ =>
        state -> None

  private def renderText(in: String, st: Prop)(state: State): String =
    val t = if state.cursor == in.length then in + "_" else in
    st.render(t.take(state.cursor)) +
      style.cursor.render(t.charAt(state.cursor).toString) +
      st.render(in.drop(state.cursor + 1))

}
