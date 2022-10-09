package p752.demo

import p752.Tile
import p752.Event
import p752.tiles.VerticalList
import p752.Style
import p752.demo.MainMenu.Menu
import p752.demo.demos.InputDemo
import p752.demo.demos.AutoCompleteDemo
import p752.demo.demos.Styles
import p752.demo.demos.TableDemo
import p752.demo.demos.SimplePromptDemo

object MainMenu:
  private val selectedStyle =
    Style.empty.copy(foreground = 16, background = 155)
  def apply(): Tile[Nothing] =
    MainMenu(vl =
      new VerticalList[MainMenu.Menu](
        MainMenu.Menu.values.toList,
        _.toString,
        selectedStyle = selectedStyle
      )
    )

  enum Menu:
    case Input extends Menu
    case Autocomplete extends Menu
    case TablePopup extends Menu
    case SimplePrompt extends Menu
    case Exit extends Menu

private case class MainMenu(vl: VerticalList[MainMenu.Menu])
    extends Tile[Nothing]:

  def handleChoos(menu: MainMenu.Menu): Tile[Nothing] = menu match
    case Menu.Input =>
      InputDemo(this)
    case Menu.Autocomplete =>
      AutoCompleteDemo(this)
    case Menu.TablePopup =>
      TableDemo(this)
    case Menu.SimplePrompt =>
      SimplePromptDemo(this)
    case Menu.Exit =>
      System.exit(0)
      this

  override def update(event: Either[Event, Nothing]): Tile[Nothing] =
    event match
      case Left(Event.Special.Enter) =>
        handleChoos(vl.selected.get)
      case Left(value) =>
        this.copy(vl.update(event))
      case Right(value) =>
        this

  override val render: String = Styles.Frames(vl.render)
