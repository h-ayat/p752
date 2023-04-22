package p752

import p752.Tiles.toLines

case class Engine[-E >: KeyEvent](comp: Tile[E]):
  private var size = 1
  private var state = comp
  def run(): Unit =
    NativeBindings.init()
    print(Sequences.cursorInvisible)
    while true do
      val content = state.render
      val l = content.toLines

      l.foreach(Engine.cleanPrintln)
      if size < l.length then size = l.length - 1
      else (l.length to size).map(_ => "").foreach(Engine.cleanPrintln)

      val next = NativeBindings.nextChar()
      if next == 3 then System.exit(0)
      val event = KeyEvent(next)
      state = state.update(event)
      Sequences.up(size + 1)

object Engine:
  import Tiles.toLines

  def cleanPrintln(c: String): Unit =
    println(Sequences.clearLine + c)

object Sequences:

  val ESC: Char = '\u001b'
  val clearLine: String = s"$ESC[2K"
  val cursorInvisible: String = s"$ESC[?25l"
  val cursorVisible: String = s"$ESC[?25h"

  def up(a: Int): Unit =
    print(s"$ESC[${a}F")
