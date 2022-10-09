package p752.demo.demos

import p752.Tile
import p752.Event
import p752.tiles.Table
import p752.tiles.Input
import p752.Tiles
import p752.demo.demos.Styles.Frames
import p752.Tiles.onTopOf
import p752.Tiles.square

object TableDemo {

  private val allData =
    TableData.allData.split("\n").map(_.split(",").toList).toList

  private val defaultTable = Table(
    headers = allData.head.map(" " + _ + " "),
    data = allData.tail,
    title = Some("Scala Collections Performance")
  )

  def apply(parent: Tile[Nothing]): Tile[Nothing] =
    TableDemo(table = defaultTable, parent, None)
}

private case class TableDemo(
    table: Table,
    parent: Tile[Nothing],
    maybeInput: Option[Input]
) extends Tile[Nothing]:
  override val render: String =
    val tableResult = table.render
    val mainResult = Styles.Frames(tableResult)
    maybeInput match
      case None => mainResult
      case Some(value) =>
        val inputResult = renderInput(value)
        inputResult.onTopOf(mainResult)

  private def renderInput(in: Input): String =
    val message = "Enter new value"
    val main = Tiles.renderVertical(message, in.render)
    main.onTopOf(frame)

  override def update(event: Either[Event, Nothing]): Tile[Nothing] =
    maybeInput match
      case None =>
        event match
          case Left(Event.Special.Backspace) =>
            parent
          case Left(Event.Special.Enter) =>
            val cell = table.data(table.y)(table.x)
            val in = Input(placeHolder = cell)
            this.copy(maybeInput = Some(in))
          case Right(_) =>
            this
          case e =>
            this.copy(table = table.update(event))
      case Some(value) =>
        event match
          case Left(Event.Special.Enter) =>
            this.copy(
              table = table.copy(data =
                table.data.updated(
                  table.y,
                  table.data(table.y).updated(table.x, value.text)
                )
              ),
              maybeInput = None
            )
          case e =>
            this.copy(maybeInput = Some(value.update(e)))

end TableDemo

private val frame = Frames(Tiles.fillSquare(" ")(20, 8))

private object TableData:
  val allData =
    """Collection,Head,Tail,Apply,Update,Prepend,Append,Notes,Version
|List,C,C,L,L,C,L,-, Scala 2.13.9 
|LazyList,C,C,L,L,C,L,-, Scala 2.13.9 
|ArraySeq,C,L,C,L,L,L,-, Scala 2.13.9 
|Vector,eC,eC,eC,eC,eC,eC,-, Scala 2.13.9 
|Queue,aC,aC,L,L,L,C,-, Scala 2.13.9 
|Range,C,C,C,-,-,-,-, Scala 2.13.9 
|String,C,L,C,L,L,L,-, Scala 2.13.9 
|Dummy List,C,C,L,L,C,L,-, Scala 2.13.9 
|Dummy LazyList,C,C,L,L,C,L,-, Scala 2.13.9 
|Dummy ArraySeq,C,L,C,L,L,L,-, Scala 2.13.9 
|Dummy Vector,eC,eC,eC,eC,eC,eC,-, Scala 2.13.9 
|Dummy Queue,aC,aC,L,L,L,C,-, Scala 2.13.9 
|Dummy Range,C,C,C,-,-,-,-, Scala 2.13.9 
|Dummy String,C,L,C,L,L,L,-, Scala 2.13.9 
""".stripMargin
