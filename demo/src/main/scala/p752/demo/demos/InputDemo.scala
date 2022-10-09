package p752.demo.demos

import p752.Tile
import p752.Event
import p752.tiles.Input
import p752.Tiles

object InputDemo:
  def apply(parent: Tile[Nothing]): Tile[Nothing] =
    val input = Input()
    InputDemo(parent, input)

end InputDemo

private case class InputDemo(parent: Tile[Nothing], input: Input)
    extends Tile[Nothing]:

  private val message = "Hit 'Enter' write exit to go back"

  override def update(event: Either[Event, Nothing]): Tile[Nothing] =
    event match
      case Left(Event.Special.Enter) =>
        parent
      case Left(_) =>
        val nextInput = input.update(event)
        if nextInput.text.toLowerCase() == "exit"
        then parent
        else this.copy(parent, nextInput)
      case Right(_) =>
        this

  override val render: String =
    Styles.Frames(
      Tiles.renderVertical(message, input.render)
    )

end InputDemo
