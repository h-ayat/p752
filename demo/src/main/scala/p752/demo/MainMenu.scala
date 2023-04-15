package p752.demo

import p752.KeyEvent
import p752.tiles.VerticalList
import p752.Prop
import p752.demo.MainMenu.Menu
import p752.demo.demos.InputDemo
import p752.EngineEvent
import p752.Tile
import p752.tiles.GenericList
import p752.demo.demos.Styles
import p752.tiles.Input
import p752.demo.demos.DemoEvent
import p752.demo.demos.DemoEvent
import GenericList.Style
import p752.demo.demos.AutoCompleteDemo
import p752.demo.demos.PromptDemo
import p752.demo.demos.DemoEvent.Ended
import p752.demo.demos.DemoEvent.Pass
import p752.demo.demos.TableDemo
import scala.scalanative.unsafe.extern
import p752.KeyEvent.Key

object MainMenu:
  private val selectedStyle =
    Prop.empty.copy(foreground = 16, background = 155)

  sealed trait State
  object State:
    case class MainMenu(list: GenericList.State[Menu]) extends State
    case class InputDemo(inputState: Input.State) extends State
    case class AutoComplete(acState: AutoCompleteDemo.State) extends State
    case class Prompt(pState: PromptDemo.State) extends State
    case class Table(tState: TableDemo.State) extends State

  val defaultState = State.MainMenu(
    GenericList.State(
      MainMenu.Menu.values.toList
    )
  )

  def apply(): Tile[KeyEvent, State, EngineEvent] =
    MainMenu(vl =
      new VerticalList[MainMenu.Menu](
        _.toString,
        style = Style(selected = selectedStyle)
      )
    )

  enum Menu:
    case Input extends Menu
    case Autocomplete extends Menu
    case TablePopup extends Menu
    case SimplePrompt extends Menu
    case Exit extends Menu

import MainMenu.State
private case class MainMenu(vl: VerticalList[MainMenu.Menu])
    extends Tile[KeyEvent, State, EngineEvent]:

  override def update(event: KeyEvent, state: State): (State, EngineEvent) =
    state match
      case s: State.MainMenu =>
        updateMainMenu(event, s)
      case State.InputDemo(s) =>
        updateInput(event, s) -> EngineEvent.Pass
      case State.AutoComplete(s) =>
        updateAutoComplete(event, s) -> EngineEvent.Pass
      case State.Prompt(s) =>
        updatePrompt(event, s) -> EngineEvent.Pass
      case State.Table(s) =>
        updateTable(event, s) -> EngineEvent.Pass

  override def render(state: State): String =
    state match
      case State.MainMenu(list) =>
        Styles.Frames(vl.render(list))
      case State.InputDemo(in) =>
        InputDemo.render(in)
      case State.AutoComplete(in) =>
        AutoCompleteDemo.render(in)
      case State.Prompt(s) =>
        PromptDemo.render(s)
      case State.Table(s) =>
        TableDemo.render(s)

  private def updateMainMenu(
      event: KeyEvent,
      state: State.MainMenu
  ): (State, EngineEvent) =
    val (newState, msg) = vl.update(event, state.list)
    msg match
      case None =>
        state.copy(newState) -> EngineEvent.Pass
      case Some(itemSelected) =>
        menuItemChosen(itemSelected.value, state)

  private def updateTable(event: KeyEvent, state: TableDemo.State): State =
    val (newState, msg) = TableDemo.update(event, state)
    msg match
      case Ended =>
        MainMenu.defaultState
      case Pass =>
        State.Table(newState)

  private def updateInput(event: KeyEvent, state: Input.State): State =
    val (newInputState, msg) = InputDemo.update(event, state)
    msg match
      case DemoEvent.Ended =>
        MainMenu.defaultState
      case DemoEvent.Pass =>
        State.InputDemo(newInputState)

  private def updateAutoComplete(
      event: KeyEvent,
      state: AutoCompleteDemo.State
  ): State =
    val (newACState, msg) = AutoCompleteDemo.update(event, state)
    msg match
      case DemoEvent.Ended =>
        MainMenu.defaultState
      case DemoEvent.Pass =>
        State.AutoComplete(newACState)

  private def updatePrompt(
      event: KeyEvent,
      state: PromptDemo.State
  ): State =
    val (newState, msg) = PromptDemo.update(event, state)
    msg match
      case Ended =>
        MainMenu.defaultState
      case Pass =>
        State.Prompt(newState)

  private def menuItemChosen(menu: Menu, state: State): (State, EngineEvent) =
    menu match
      case Menu.Input =>
        State.InputDemo(Input.defaultState) -> EngineEvent.Pass
      case Menu.Autocomplete =>
        State.AutoComplete(AutoCompleteDemo.defaultState) -> EngineEvent.Pass
      case Menu.SimplePrompt =>
        State.Prompt(PromptDemo.defaultState) -> EngineEvent.Pass
      case Menu.TablePopup =>
        State.Table(TableDemo.defaultState) -> EngineEvent.Pass
      case Menu.Exit =>
        state -> EngineEvent.Terminate
