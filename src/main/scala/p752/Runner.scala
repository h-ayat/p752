package p752

import cons.NativeBindings
import p752.Tile
import p752.Event
import p752.Engine
import p752.Event.Key
import p752.Style
import p752.Tiles._
import p752.Sequences
import p752.Border
import p752.Event.Special.Enter
import p752.tiles.VerticalList
import p752.tiles.Input

object Main:
  def main(args: Array[String]): Unit =
    val eng = new Engine(Input())
    eng.run()
