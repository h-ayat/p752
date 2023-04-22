package p752.tiles

import p752.Style
import p752.Tile
import p752.KeyEvent
import p752.Tiles._

object Input {
  val delimiters: Set[Char] = " -!@#&()[{}]:;',?/*\"`~$^+=<>".toSet
}

final case class Input(
    text: String = "",
    placeHolder: String = "",
    style: Style = Style.empty,
    placeHolderStyle: Style = Style.empty,
    cursorStyle: Style = Style(background = 236, blinking = true),
    cursor: Int = 0
) extends Tile[KeyEvent]:

  private def renderText(in: String, st: Style): String =
    val t = if cursor == in.length then in + "_" else in
    st.render(t.take(cursor)) +
      cursorStyle.render(t.charAt(cursor).toString) +
      st.render(in.drop(cursor + 1))

  override val render: String =
    if text.isEmpty then renderText(placeHolder, placeHolderStyle)
    else renderText(text, style)

  override def update(event: KeyEvent): Input = event match
    case KeyEvent.Key(ch) =>
      val t = text.take(cursor) + ch + text.drop(cursor)
      this.copy(text = t, cursor = cursor + 1)
    case KeyEvent.Special.ETB =>
      val killStart = (1 to (cursor - 1)).reverse
        .find { i =>
          Input.delimiters.contains(text(i - 1))
        }
        .getOrElse(0)
      val finalText =
        text.substring(0, killStart) + text.substring(cursor)
      copy(text = finalText, cursor = killStart)
    case KeyEvent.Special.Left =>
      val c = if cursor == 0 then 0 else cursor - 1
      copy(cursor = c)
    case KeyEvent.Special.Right =>
      val c = if cursor == text.length() then cursor else cursor + 1
      copy(cursor = c)
    case KeyEvent.Special.Backspace =>
      val t = text.take(cursor - 1) + text.drop(cursor)
      val c = if cursor == 0 then 0 else cursor - 1
      copy(text = t, cursor = c)
    case KeyEvent.Special.Del =>
      val t = text.take(cursor) + text.drop(cursor + 1)
      copy(text = t)
    case _ =>
      this
