package p752.demo.demos

import p752.KeyEvent

import p752.Tile
import p752.KeyEvent
import p752.tiles.Input
import p752.Tiles

object InputDemo extends Tile[KeyEvent, Input.State, DemoEvent]:
  private val message = "Hit 'Enter' write exit to go back"
  private val input = Input()

  override def update(
      event: KeyEvent,
      state: Input.State
  ): (Input.State, DemoEvent) =
    event match
      case KeyEvent.Special.Enter =>
        state -> DemoEvent.Ended
      case event =>
        val nextInput = input.update(event, state)._1
        if nextInput.text.toLowerCase() == "exit"
        then state -> DemoEvent.Ended
        else nextInput -> DemoEvent.Pass

  override def render(state: Input.State): String =
    Styles.Frames(
      Tiles.renderVertical(message, input.render(state))
    )
end InputDemo

