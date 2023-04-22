package p752.demo

import p752.KeyEvent.Key
import p752.KeyEvent.Special.Enter
import p752.Tiles.*
import p752.tiles.{Input, VerticalList}
import p752.{Border, Engine, KeyEvent, Sequences, Style, Tile}

object Runner:
  def main(args: Array[String]): Unit =
    val mainMenu = MainMenu()
    val eng = new Engine(mainMenu)
    eng.run()
