package p752

import p752.Tiles.toLines
import scala.annotation.tailrec

case class Engine[-E >: KeyEvent, S, +MSG <: EngineEvent](
    comp: Tile[E, S, MSG],
    initialState: S
):
  def run(): Unit =
    NativeBindings.init()
    print(Sequences.cursorInvisible)
    go(initialState, 1)
    print(Sequences.cursorVisible)

  @tailrec
  private def go(state: S, size: Int): Unit = {
    val content = comp.render(state)
    val l = content.toLines

    l.foreach(Engine.cleanPrintln)
    val newSize =
      if size < l.length
      then l.length - 1
      else
        (l.length to size).map(_ => "").foreach(Engine.cleanPrintln)
        size

    val next = NativeBindings.nextChar()
    if next != 3
    then
      val event = KeyEvent(next)
      val (newState, msg) = comp.update(event, state)
      Sequences.up(newSize + 1)

      msg match {
        case EngineEvent.Terminate => ()
        case EngineEvent.Pass =>
          go(newState, newSize)
      }
    else ()
  }

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

sealed trait EngineEvent
object EngineEvent {
  case object Terminate extends EngineEvent
  case object Pass extends EngineEvent
}
