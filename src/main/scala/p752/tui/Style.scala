package p752.tui

import p752.tui.StringUtils.toLines

trait Alter:
  def render(content: String): String

case class Style(
    foreground: Int = 255,
    background: Int = 16,
    italic: Boolean = false,
    bold: Boolean = false,
    underlined: Boolean = false,
    blinking: Boolean = false,
    dim: Boolean = false,
    striked: Boolean = false
) extends Alter:
  import StringUtils._
  import Style.Codes
  def render(content: String): String =
    var s = ""
    if italic then s = Codes.italic + s
    if bold then s = Codes.bold + s
    if underlined then s = Codes.underlined + s
    if blinking then s = Codes.blinking + s
    if dim then s = Codes.dim + s
    if striked then s = Codes.striked + s

    content.toLines
      .map { line =>
        Codes.fColor(foreground) +
          Codes.bColor(background) +
          s +
          line +
          Codes.reset
      }
      .mkString("\n")
  end render
end Style

case class Padding(top: Int = 0, left: Int = 0, bottom: Int = 0, right: Int = 0)
    extends Alter:
  def render(content: String): String =
    import StringUtils._

    val s = content.square.toLines.map(" ".times(left) + _ + " ".times(right))
    val len = s.head.pureSize

    val emptyLine = " ".times(len) + "\n"

    emptyLine.times(top) + s.mkString + "\n" + emptyLine.times(top).init

case class Border(style: Style = Style(), round: Boolean = false) extends Alter:
  import StringUtils._

  val floor = "─"
  val wall = "│"
  val rounds = "╭╮╰╯"
  val normal = "┌┐└┘"

  def render(content: String): String =
    val angles = if round then rounds else normal
    val s = content.square.toLines
    val len = s.head.pureSize
    val top = style.render(angles(0) + floor.times(len) + angles(1))
    val bottom = style.render(angles(2) + floor.times(len) + angles(3))

    ((top :: s.map(style.render(wall) + _ + style.render(wall)) ++ List(
      bottom
    )))
      .mkString("\n")

object Style:
  import Sequences.ESC
  object Codes:
    val bold = ESC + "[1m"
    val dim = ESC + "[2m"
    val italic = ESC + "[3m"
    val underlined = ESC + "[4m"
    val blinking = ESC + "[5m"
    val striked = ESC + "[9m"
    val reset = s"$ESC[0m"

    def fColor(a: Int): String = s"$ESC[38;5;${a}m"
    def bColor(a: Int): String = s"$ESC[48;5;${a}m"

object Align:
  sealed trait Vertical
  object Vertical:
    case object Top extends Vertical
    case object Center extends Vertical
    case object Bottom extends Vertical
    val all = Top :: Center :: Bottom :: Nil

  sealed trait Horizontal
  object Horizontal:
    case object Left extends Horizontal
    case object Center extends Horizontal
    case object Right extends Horizontal
    val all = Left :: Center :: Right :: Nil
