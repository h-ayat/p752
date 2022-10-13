package p752

import cons.NativeBindings
import p752.Tile
import p752.Event
import p752.Engine
import p752.Event.Key
import p752.Style
import p752.samples.{
  ThreeRandomTiles,
  SquareRandomTiles,
  RandomTile,
  RandomOverlay
}
import p752.Tiles._
import p752.Sequences
import p752.Border
import p752.Event.Special.Enter
import p752.samples.Rand
import p752.tiles.VerticalList

object Main:
  def main(args: Array[String]): Unit =
    val list = "a,bc,def,ghi".split(',').toList

    val eng = new Engine(VerticalList(list, identity, selectedStyle = Style(background = 241)))
    eng.run()
