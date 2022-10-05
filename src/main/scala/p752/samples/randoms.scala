package p752.samples

import p752.tui.Tile
import p752.tui.{Style, Border}
import p752.tui.Event
import scala.util.Random.nextInt
import p752.tui.Event.Key
import p752.tui.StringUtils._
import p752.tui.Border
import p752.tui.Align

class RandomTile(content: String, fg: Int = 141, bg: Int = 232) extends Tile:

  val textStyle = Style(foreground = fg, background = bg)
  val style = Style(blinking = true)
  val border = Border(style, true)

  override def render(): String =
    border.render(textStyle.render(content.square))

  override val update: PartialFunction[Event, RandomTile] =
    case e: Event =>
      val next = (1 to (3 + nextInt(7)))
        .map(_ =>
          (1 to (5 + nextInt(15)))
            .map(_ => scala.util.Random.nextPrintableChar())
            .mkString("")
        )
        .mkString("\n")
      val message = e match {
        case Key(ch) => ch.toString()
        case any     => any.toString()
      }
      val fg = nextInt(255)
      val bg = nextInt(255)
      RandomTile(s"$message $fg $bg $next", nextInt(255), nextInt(255))

case class ThreeRandomTiles(r1: RandomTile, r2: RandomTile, r3: RandomTile)
    extends Tile {

  override def render(): String =
    Tile.renderVertical(r1.render(), r2.render(), r3.render())

  override val update: PartialFunction[Event, ThreeRandomTiles] =
    case e: Event =>
      val newR1 = r1.update(e)
      val newR2 = r2.update(e)
      val newR3 = r3.update(e)
      this.copy(newR1, newR2, newR3)
}

case class SquareRandomTiles(
    r1: ThreeRandomTiles,
    r2: ThreeRandomTiles,
    r3: ThreeRandomTiles
) extends Tile {

  override def render(): String =
    Tile.renderHorizontal(r1.render(), r2.render(), r3.render())
  override val update: PartialFunction[Event, SquareRandomTiles] =
    case e: Event =>
      val nr1 = r1.update(e)
      val nr2 = r2.update(e)
      val nr3 = r3.update(e)
      this.copy(nr1, nr2, nr3)

}

class RandomOverlay(back: Tile, visible: Boolean = true) extends Tile {
  val border = Border(style = Style(foreground = 199))
  val frame = border.render(empty(14 + nextInt(12), 4 + nextInt(6)))
  override def render(): String =
    if visible then
      frame.onTopOf(
        back.render(),
        va = Rand.choose(Align.Vertical.all: _*),
        vh = Rand.choose(Align.Horizontal.all: _*)
      )
    else back.render()

  override val update: PartialFunction[Event, Tile] = { case e =>
    val b = back.update(e)
    new RandomOverlay(b)
  }

}

object Rand:
  def choose[T](ts: T*): T =
    ts(nextInt(ts.length))
