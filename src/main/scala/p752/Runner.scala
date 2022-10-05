package p752

import cons.NativeBindings
import p752.tui.Tile
import p752.tui.Event
import p752.tui.Engine
import p752.tui.Event.Key
import p752.tui.Style
import p752.samples.{ThreeRandomTiles, SquareRandomTiles, RandomTile,RandomOverlay}
import p752.tui.StringUtils._
import p752.tui.Sequences
import p752.tui.Border
import p752.tui.Event.Special.Enter

object Main:
  def main(args: Array[String]): Unit =
    val r1 = new RandomTile("R1")
    val r2 = new RandomTile("R2")
    val r3 = new RandomTile("R3")
    val randoms1 = ThreeRandomTiles(r1, r2, r3)
    val sq = SquareRandomTiles(randoms1, randoms1.copy(), randoms1.copy())
    val overlay = new RandomOverlay(sq.update(Event.Special.Enter))

    val eng = new Engine(overlay)
    eng.run()
