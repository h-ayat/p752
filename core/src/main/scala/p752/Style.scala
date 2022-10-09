package p752

import p752.Tiles.toLines

trait Alter:
  self =>
  def render(content: String): String
  def <|(other: Alter): Alter = new Alter {
    override def render(content: String): String = 
      self.render(other.render(content))

  }


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
  import Tiles._
  import Style.Codes

  private val styleSeq = {
    val buffer = scala.collection.mutable.ListBuffer[String]()
    if italic then buffer.append(Codes.italic)
    if bold then buffer.append(Codes.bold)
    if underlined then buffer.append(Codes.underlined)
    if blinking then buffer.append(Codes.blinking)
    if dim then buffer.append(Codes.dim)
    if striked then buffer.append(Codes.stroked)
    buffer.append(Codes.fColor(foreground))
    buffer.append(Codes.bColor(background))
    Sequences.ESC + "[" + buffer.mkString(";") + "m"
  }
  def render(content: String): String =
    content.toLines
      .map { line =>
        styleSeq + line + Codes.reset
      }
      .mkString("\n")
  end render
end Style

case class Padding(top: Int = 0, left: Int = 0, bottom: Int = 0, right: Int = 0)
    extends Alter:
  def render(content: String): String =
    import Tiles._

    val s = content.square.toLines.map(" ".times(left) + _ + " ".times(right))
    val len = s.head.pureSize

    val emptyLine = " ".times(len) + "\n"

    emptyLine.times(top) + s.mkString("\n") + "\n" + emptyLine.times(top).init

case class Border(style: Style = Style(), round: Boolean = false) extends Alter:
  import Tiles._

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

    (top :: s.map(style.render(wall) + _ + style.render(wall)) ++ List(
      bottom
    ))
      .mkString("\n")

object Style:
  import Sequences.ESC
  val empty: Style = Style()
  object Codes:
    val bold = "1"
    val dim = "2"
    val italic = "3"
    val underlined = "4"
    val blinking = "5"
    val stroked = "9"
    val reset = s"$ESC[0m"

    def fColor(a: Int): String = s"38;5;$a"
    def bColor(a: Int): String = s"48;5;$a"

object Align:
  sealed trait Vertical
  object Vertical:
    case object Top extends Vertical
    case object Center extends Vertical
    case object Bottom extends Vertical
    val all: Seq[Vertical] = Top :: Center :: Bottom :: Nil

  sealed trait Horizontal
  object Horizontal:
    case object Left extends Horizontal
    case object Center extends Horizontal
    case object Right extends Horizontal
    val all: Seq[Horizontal] = Left :: Center :: Right :: Nil
