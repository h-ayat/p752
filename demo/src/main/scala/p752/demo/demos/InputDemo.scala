package p752.demo.demos

import p752.Tile
import p752.KeyEvent
import p752.tiles.Input
import p752.Tiles

object InputDemo:
  def apply(parent: Tile[KeyEvent]): Tile[KeyEvent] =
    val input = Input()
    InputDemo(parent, input)

end InputDemo

private case class InputDemo(parent: Tile[KeyEvent], input: Input)
    extends Tile[KeyEvent]:

  private val message = "Hit 'Enter' write exit to go back"

  override def update(event: KeyEvent): Tile[KeyEvent] =
    event match
      case KeyEvent.Special.Enter =>
        parent
      case _ =>
        val nextInput = input.update(event)
        if nextInput.text.toLowerCase() == "exit"
        then parent
        else this.copy(parent, nextInput)

  override val render: String =
    Styles.Frames(
      Tiles.renderVertical(message, input.render)
    )

end InputDemo
