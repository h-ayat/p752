package p752


sealed trait KeyEvent

object KeyEvent:
  def apply(ch: Int): KeyEvent =
    Special.map.getOrElse(ch.intValue, Key(ch.toChar))

  case class Key(ch: Char) extends KeyEvent

  sealed trait Special extends KeyEvent:
    val code: Int

  object Special:
    case object Tab extends Special { val code = 9 }
    case object Backspace extends Special { val code = 127 }
    case object Enter extends Special { val code = 10 }
    case object Left extends Special { val code = 268 }
    case object Right extends Special { val code = 267 }
    case object Up extends Special { val code = 265 }
    case object Down extends Special { val code = 266 }

    case object Del extends Special { val code = 280 }

    /** End of Transmission block (Ctrl+W)
      */
    case object ETB extends Special { val code = 23 }

    val all: Seq[Special] =
      ETB :: Tab :: Backspace :: Enter :: Left :: Right :: Up :: Down :: Del :: Nil
    val map: Map[Int, Special] = all.map(t => t.code -> t).toMap
