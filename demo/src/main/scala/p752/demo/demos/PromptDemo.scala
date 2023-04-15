package p752.demo.demos

import p752.Tile
import p752.KeyEvent
import p752.tiles.Prompt
import p752.KeyEvent.Special
import p752.tiles.GenericList


object PromptDemo extends Tile[KeyEvent, PromptDemo.State, DemoEvent]:

  type State = GenericList.State[String]

  private val message = "Can you answer a simple question ?"
  private val options = List("Yes", "No", "Maybe", "But of course", "Hmmm")

  val defaultState: State = GenericList.State(options)
  private val prompt = new Prompt[String](message, identity)

  override def update(event: KeyEvent, state: State): (State, DemoEvent) =
    val (newState, msg) = prompt.update(event, state)
    newState -> (msg match
      case None =>
        DemoEvent.Pass
      case Some(_) =>
        DemoEvent.Ended
    )

  override def render(state: State): String =
    Styles.Frames.padding.render(prompt.render(state))
