package p752.tui

import cons.NativeBindings
import javax.sound.midi.Sequence
import p752.tui.StringUtils.toLines

case class Engine(comp: Tile):
  var size = 1
  var state = comp
  def run(): Unit =
    NativeBindings.init()
    print(Sequences.cursorInvisible)
    while true do
      val content = state.render()
      val l = content.toLines

      l.foreach(Engine.cleanPrintln)
      if size < l.length then size = l.length - 1
      else (l.length to size).map(_ => "").foreach(Engine.cleanPrintln)

      val next = NativeBindings.nextChar()
      if next == 3 then System.exit(0)
      val event = Event(next)
      state = state.update.applyOrElse(event, _ => state)
      Sequences.up(size + 1)

object Engine:
  import StringUtils.{toLines}

  def cleanPrintln(c: String): Unit =
    println(Sequences.clearLine + c)

object Sequences:

  val ESC = '\u001b'
  val clearLine = s"$ESC[2K"
  val cursorInvisible = s"$ESC[?25l"
  val cursorVisible = s"$ESC[?25h"

  def up(a: Int): Unit =
    print(s"${ESC}[${a}F")
