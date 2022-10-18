package p752

import p752.Event.Key
import p752.Event.Special.Enter
import p752.Tiles.*
import p752.tiles.{Input, VerticalList}
import p752.{Border, Engine, Event, Sequences, Style, Tile}

object Main:
  def main(args: Array[String]): Unit =
    val eng = new Engine(Input())
    eng.run()
