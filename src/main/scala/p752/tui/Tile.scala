package p752.tui

import p752.tui.StringUtils.toLines
import p752.tui.StringUtils.pureSize
import p752.tui.StringUtils.fillSquare

trait Tile:
  def render(): String
  val update: PartialFunction[Event, Tile]

object Tile:
  def renderVertical(items: String*): String =
    val h = items.map(_.toLines.length).max
    val squaredItems = items.map { item =>
      val w = item.toLines.map(_.pureSize).max
      item.fillSquare(w, h)
    }
    val all = squaredItems.toList.map(_.toLines)
    (0 until h)
      .map(index => all.map(_.apply(index)).reduce(_ + _))
      .mkString("\n")

  def renderHorizontal(items: String*): String =
    items.mkString("\n")
