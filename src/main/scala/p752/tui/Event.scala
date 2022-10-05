package p752.tui

sealed trait Event

object Event:
  def apply(ch: Int): Event = Special.map.getOrElse(ch.intValue, Key(ch.toChar))

  case class Key(ch: Char) extends Event

  sealed trait Special extends Event:
    val code: Int

  object Special:
    case object Tab extends Special { val code = 9 }
    case object Backspace extends Special { val code = 127 }
    case object Enter extends Special { val code = 10 }
    case object Left extends Special { val code = 268 }
    case object Right extends Special { val code = 267 }
    case object Up extends Special { val code = 265 }
    case object Down extends Special { val code = 266 }

    val all = Tab :: Backspace :: Enter :: Left :: Right :: Up :: Down :: Nil
    val map = all.map(t => t.code -> t).toMap
