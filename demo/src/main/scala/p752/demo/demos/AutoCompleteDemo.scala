package p752.demo.demos

import p752.Tile
import p752.tiles.AutoComplete
import p752.Event

object AutoCompleteDemo:
  private val scalaReservedWords =
    """abstract, case, catch, class, def, do, else, extends, false, final, finally, for, forSome, if, implicit, import, lazy, match, new, null, object, override, package, private, protected, return, sealed, super, this, throw, trait, try, true, type, val, var, while, with, yield"""
      .replaceAllLiterally(" ", "")
      .split(",")
      .toList

  def apply(parent: Tile[Nothing]): Tile[Nothing] =
    val ac = AutoComplete[String](
      items = scalaReservedWords,
      renderItem = identity
    )
    AutoCompleteDemo(parent, ac)

private case class AutoCompleteDemo(
    parent: Tile[Nothing],
    ac: AutoComplete[String]
) extends Tile[Nothing]:
  override val render: String =
    Styles.Frames(
      ac.render
    )

  override def update(event: Either[Event, Nothing]): Tile[Nothing] =
    event match
      case Left(Event.Special.Enter) =>
        parent
      case e: Left[Event, Nothing] =>
        val ac2 = ac.update(e)
        this.copy(ac = ac2)
      case Right(_) =>
        this
