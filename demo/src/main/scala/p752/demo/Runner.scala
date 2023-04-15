package p752.demo

import p752.Engine

object Runner:
  def main(args: Array[String]): Unit =
    val mainMenu = MainMenu()
    val eng = new Engine(mainMenu, MainMenu.defaultState)
    eng.run()
