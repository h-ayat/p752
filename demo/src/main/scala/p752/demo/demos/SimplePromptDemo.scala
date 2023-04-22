package p752.demo.demos

import p752.Tile
import p752.KeyEvent
import p752.tiles.Prompt
import p752.KeyEvent.Special

object SimplePromptDemo:
  private val prompt = Prompt[String](
    message = "Can you answer a simple question ?",
    options = List("Yes", "No", "Maybe", "But of course", "Hmmm"),
    show = identity[String]
  )
  def apply(parent: Tile[KeyEvent]): Tile[KeyEvent] =
    SimplePromptDemo(prompt, parent)

private final case class SimplePromptDemo(
    prompt: Prompt[String],
    parent: Tile[KeyEvent]
) extends Tile[KeyEvent]:

  override def update(event: KeyEvent ): Tile[KeyEvent] =
    event match
      case KeyEvent.Special.Enter =>
        parent
      case _ =>
        this.copy(prompt = prompt.update(event))

  override val render: String = Styles.Frames.padding.render(prompt.render)
