package p752.demo.demos

import p752.Tile
import p752.KeyEvent
import p752.tiles.Table
import scala.scalanative.unsafe.extern
import p752.tiles.Input
import p752.demo.demos.TableDemo.State.Normal
import p752.demo.demos.TableDemo.State.DataEntry
import p752.KeyEvent.Special.Enter
import p752.KeyEvent.Special.ETB
import p752.KeyEvent.Special.Backspace
import p752.Padding
import p752.Border
import p752.Prop
import p752.Tiles.onTopOf
import p752.Util

object TableDemo extends Tile[KeyEvent, TableDemo.State, DemoEvent]:

  type Data = List[String]
  sealed trait State
  object State:
    case class Normal(tableState: Table.State[Data]) extends State
    case class DataEntry(inputState: Input.State, tableState: Table.State[Data])
        extends State

  private val table =
    new Table[Data](
      headers = TableData.allData.head,
      identity,
      Some("Scala 2 Collections Performance")
    )
  private val input = new Input(placeHolder = "Enter new value here")
  private val totalAlter = {
    val p = Padding(1, 2, 1, 2)
    val b = Border(Prop.empty, true)
    p <| b <| p
  }

  private val popupAlter = {
    val borderStyle = Prop(197, 16)
    val p = Padding(1, 2, 1, 2)
    val b = Border(borderStyle, true)
    p <| b <| p

  }

  val defaultState = Normal(Table.State(TableData.allData))

  private val message = {
    "\n\n" + Prop(foreground = 241, italic = true).render(
      "Press 'q' or 'Backspace' to go back."
    )
  }

  override def render(state: State): String =
    val base = state match
      case Normal(tableState) =>
        table.render(tableState)
      case DataEntry(inputState, tableState) =>
        val in = popupAlter.render(input.render(inputState))
        val tab = table.render(tableState)
        in.onTopOf(tab)
    totalAlter.render(base + message)

  override def update(event: KeyEvent, state: State): (State, DemoEvent) =
    state match
      case Normal(tableState) =>
        event match
          case Enter =>
            val inputState = Input.State("")
            DataEntry(inputState, tableState) -> DemoEvent.Pass
          case Backspace | KeyEvent.Key('q') =>
            state -> DemoEvent.Ended
          case _ =>
            val newState = table.update(event, tableState)._1
            Normal(newState) -> DemoEvent.Pass

      case DataEntry(inputState, tableState) =>
        event match
          case Enter =>
            val newData = Util.changeAt(
              tableState.y,
              tableState.raw,
              { row =>
                Util.changeAt(tableState.x, row, _ => inputState.text)
              }
            )
            Normal(tableState.copy(raw = newData)) -> DemoEvent.Pass
          case _ =>
            val newInState = input.update(event, inputState)._1
            DataEntry(newInState, tableState) -> DemoEvent.Pass

private object TableData:
  val allData: List[List[String]] =
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
""".stripMargin.split("\n").toList.map(_.split(",").toList)
