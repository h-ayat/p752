package p752.demo

import p752.Event.Key
import p752.Event.Special.Enter
import p752.Tiles.*
import p752.tiles.{Input, VerticalList}
import p752.{Border, Engine, Event, Sequences, Style, Tile}

object Runner:
  def main(args: Array[String]): Unit =
    val mainMenu = MainMenu()
    val eng = new Engine(mainMenu)
    eng.run()
