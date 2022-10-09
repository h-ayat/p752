package p752.demo.demos

import p752.Tile
import p752.Event
import p752.tiles.Prompt
import p752.Event.Special

object SimplePromptDemo:
  private val prompt = Prompt[String](
    message = "Can you answer a simple question ?",
    options = List("Yes", "No", "Maybe", "But of course", "Hmmm"),
    show = identity[String]
  )
  def apply(parent: Tile[Nothing]): Tile[Nothing] =
    SimplePromptDemo(prompt, parent)

private final case class SimplePromptDemo(
    prompt: Prompt[String],
    parent: Tile[Nothing]
) extends Tile[Nothing]:

  override def update(event: Either[Event, Nothing]): Tile[Nothing] =
    event match
      case Left(Event.Special.Enter) =>
        parent
      case e =>
        this.copy(prompt = prompt.update(e))

  override val render: String = Styles.Frames.padding.render(prompt.render)
