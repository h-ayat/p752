package p752.demo.demos

import p752.Tile
import p752.tiles.AutoComplete
import p752.KeyEvent
import p752.tiles.AutoComplete.Event.Pass
import p752.tiles.AutoComplete.Event.ItemMatched
import p752.tiles.AutoComplete.Event.ItemSelected

object AutoCompleteDemo
    extends Tile[KeyEvent, AutoCompleteDemo.State, DemoEvent]:

  type State = AutoComplete.State[String]
  lazy val defaultState = AutoComplete.defaultState(scalaReservedWords)
  private val ac: AutoComplete[String] = AutoComplete[String](identity)

  override def render(state: State): String =
    Styles.Frames(
      ac.render(state)
    )

  override def update(event: KeyEvent, state: State): (State, DemoEvent) =
    val (s, e) = ac.update(event, state)
    s -> (e match
      case Pass            => DemoEvent.Pass
      case ItemMatched(_)  => DemoEvent.Pass
      case ItemSelected(_) => DemoEvent.Ended
    )

  private val scalaReservedWords =
    """abstract, case, catch, class, def, do, else, extends, false, final, finally, for, forSome, if, implicit, import, lazy, match, new, null, object, override, package, private, protected, return, sealed, super, this, throw, trait, try, true, type, val, var, while, with, yield"""
      .replaceAllLiterally(" ", "")
      .split(",")
      .toList
