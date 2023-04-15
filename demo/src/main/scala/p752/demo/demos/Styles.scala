package p752.demo.demos

import p752.{Border, Prop}
import p752.Padding

object Styles:
  val pink = 197

  object Frames:
    val border = Border(Prop(foreground = pink))
    val padding = Padding(1, 3, 1, 3)
    val alter = padding <| border <| padding
    def apply(rendered: String): String =
      alter.render(rendered)

end Styles
