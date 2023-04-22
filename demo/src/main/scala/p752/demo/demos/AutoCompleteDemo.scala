package p752.demo.demos

import p752.Tile
import p752.tiles.AutoComplete
import p752.KeyEvent

object AutoCompleteDemo:
  private val scalaReservedWords =
    """abstract, case, catch, class, def, do, else, extends, false, final, finally, for, forSome, if, implicit, import, lazy, match, new, null, object, override, package, private, protected, return, sealed, super, this, throw, trait, try, true, type, val, var, while, with, yield"""
      .replaceAllLiterally(" ", "")
      .split(",")
      .toList

  def apply(parent: Tile[KeyEvent]): Tile[KeyEvent] =
    val ac = AutoComplete[String](
      items = scalaReservedWords,
      renderItem = identity
    )
    AutoCompleteDemo(parent, ac)

private case class AutoCompleteDemo(
    parent: Tile[KeyEvent],
    ac: AutoComplete[String]
) extends Tile[KeyEvent]:
  override val render: String =
    Styles.Frames(
      ac.render
    )

  override def update(event:KeyEvent): Tile[KeyEvent] =
    event match
      case KeyEvent.Special.Enter =>
        parent
      case _ =>
        val ac2 = ac.update(event)
        this.copy(ac = ac2)
