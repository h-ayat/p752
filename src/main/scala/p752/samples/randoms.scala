package p752.samples

import p752.Tile
import p752.{Style, Border}
import p752.Event
import scala.util.Random.nextInt
import p752.Event.Key
import p752.Tiles._
import p752.Border
import p752.Align
import p752.tiles.HorizontalList
import p752.tiles.Table
import p752.Tiles

class RandomTile(content: String, fg: Int = 141, bg: Int = 232) extends Tile:

  private val textStyle = Style(foreground = fg, background = bg)
  private val style = Style(blinking = true)
  private val border = Border(style, true)

  override val render: String =
    border.render(textStyle.render(content.square))

  override def update(e: Event): RandomTile =
    val next = (1 to (3 + nextInt(7)))
      .map(_ =>
        (1 to (5 + nextInt(15)))
          .map(_ => scala.util.Random.nextPrintableChar())
          .mkString("")
      )
      .mkString("\n")
    val message = e match {
      case Key(ch) => ch.toString
      case any     => any.toString
    }
    val fg = nextInt(255)
    val bg = nextInt(255)
    RandomTile(s"$message $fg $bg $next", nextInt(255), nextInt(255))

case class ThreeRandomTiles(r1: RandomTile, r2: RandomTile, r3: RandomTile)
    extends Tile {

  override val render: String =
    Tiles.renderVertical(r1.render, r2.render, r3.render)

  override def update(e: Event): ThreeRandomTiles =
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

  override val render: String =
    Tiles.renderHorizontal(r1.render, r2.render, r3.render)
  override def update(e: Event): SquareRandomTiles =
    val nr1 = r1.update(e)
    val nr2 = r2.update(e)
    val nr3 = r3.update(e)
    this.copy(nr1, nr2, nr3)

}

case class RandomOverlay(
    back: Tile,
    visible: Boolean = true,
    list: Option[HorizontalList[String]] = None
) extends Tile {
  private val border = Border(style = Style(foreground = 199))
  private val frameW = 24 + nextInt(12)
  private val frame = border.render(empty(frameW, 4 + nextInt(6)))

  val data = (1 to 8).map { j =>
    (1 to 4)
      .map { i =>
        (j * 10 + i).toString() + "Z".times(nextInt(10))
      }
      .mkString(",")
  }.toList
  private val table =
    Table[String](
      data,
      "A,B,C,D".split(",").toList,
      _.split(",").toList,
      align = Align.Horizontal.Center
    )

  private val internal = list.getOrElse(
    new HorizontalList(
      List("Yes", "No", "Maybe"),
      identity,
      HorizontalList.Spacing.Seperator(" "), // .FillWidth(frameW),
      selectedStyle = Style(foreground = 16, background = 219)
    )
  )

  override val render: String =
    val above =
      if visible then
        internal.render
          .onTopOf(frame)
          .onTopOf(
            back.render,
            va = Rand.choose(Align.Vertical.all: _*),
            vh = Rand.choose(Align.Horizontal.all: _*)
          )
      else back.render
    Tiles.renderHorizontal(above, table.render)

  override def update(e: Event): RandomOverlay =
    val newInt = internal.update(e)
    if newInt.finished then
      val b = back.update(e)
      RandomOverlay(b, visible = !visible)
    else this.copy(list = Some(newInt))

}

object Rand:
  def choose[T](ts: T*): T =
    ts(nextInt(ts.length))
