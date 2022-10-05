package p752.tui

object StringUtils:
  def empty(w: Int, h: Int): String =
    (1 to h).map(_ => " ".times(w)).mkString("\n")

  extension (s: String) def toLines = s.split("\n").toList

  extension (s: String)
    def times(n: Int): String = (1 to n).map(_ => s).mkString("")

  extension (s: String)
    def square: String = {
      val lines = s.toLines
      val max = lines.map(_.pureSize).max

      lines.map(line => line + " ".times(max - line.pureSize)).mkString("\n")
    }

  extension (mainContent: String)
    def onTopOf(
        behind: String,
        va: Align.Vertical = Align.Vertical.Center,
        vh: Align.Horizontal = Align.Horizontal.Center
    ): String =
      val (bw, bh) = behind.dimension
      val (mw, mh) = mainContent.dimension

      val fitBehind =
        if bw < mw || bh < mh then
          behind.onTopOf(empty(Math.max(bw, mw), Math.max(bh, mh)), va, vh)
        else behind.square

      val (w, h) = fitBehind.dimension
      val (dw, dh) = (w - mw) -> (h - mh)

      val (left, right) = vh match {
        case Align.Horizontal.Left =>
          dw -> 0
        case Align.Horizontal.Center =>
          (dw / 2) -> (dw - (dw / 2))
        case Align.Horizontal.Right =>
          0 -> dw
      }

      val (above, below) = va match {
        case Align.Vertical.Top =>
          dh -> 0
        case Align.Vertical.Center =>
          (dh / 2) -> (dh - (dh / 2))
        case Align.Vertical.Bottom =>
          0 -> dh
      }
      // println(s"$mw $w $left $right")

      var behindLines = fitBehind.toLines

      val topPart = behindLines.take(above)
      behindLines = behindLines.drop(above)
      val bottomPart = behindLines.drop(mh)

      val middle = mainContent.toLines.zip(behindLines.take(mh)).map {
        case (front, back) =>
          back.pureTake(left) + front + back.pureTakeLast(right)
      }

      (topPart ++ middle ++ bottomPart).mkString("\n")
    end onTopOf

  extension (s: String)
    def dimension: (Int, Int) =
      val lines = s.toLines
      lines.map(_.pureSize).max -> lines.length

  extension (s: String)
    def fillSquare(w: Int, h: Int): String = {
      val lines = s.toLines.map { line =>
        val diff = w - line.pureSize
        line + " ".times(diff)
      }
      (lines ++ (1 to h - lines.length).map(_ => " ".times(w))).mkString("\n")
    }

  extension (s: String)
    def pureSize: Int =
      var counter = 0
      var remaining = s
      while (remaining != "") {
        if remaining.head == Sequences.ESC then
          while (!commandEndChars.contains(remaining.head)) {
            remaining = remaining.tail
          }
        else
          counter += 1
        remaining = remaining.tail

      }
      counter
      // s.replaceAll("(\\x9B|\\x1B\\[)[0-?]*[ -\\/]*[@-~]", "").length()

  private val commandEndChars: Set[Char] = (('a' to 'z') ++ ('A' to 'Z')).toSet

  extension (s: String)
    def pureTakeLast(n: Int): String =
      if n == 0 then ""
      else
        var toDrop = s.pureSize - n
        var commandBuffer = ""

        var remaining = s
        while (
          remaining.head == Sequences.ESC || (toDrop > 0 && remaining != "")
        ) {
          val ch = remaining.head

          if ch == Sequences.ESC then {
            var command = ""
            while (!commandEndChars.contains(remaining.head)) {
              command += remaining.head
              remaining = remaining.tail
            }
            command += remaining.head
            remaining = remaining.tail

            if command == Style.Codes.reset then commandBuffer = ""
            else commandBuffer += command

          } else {
            toDrop -= 1
            remaining = remaining.tail
          }
        }
        commandBuffer + remaining

  extension (s: String)
    def pureTake(n: Int): String =
      if n == 0 then ""
      else
        var mustReset = false
        var result = ""
        var counter = 0
        var remaining = s
        while (counter < n && remaining != "") {
          val ch = remaining.head

          if ch == Sequences.ESC then {
            var command = ""
            while (!commandEndChars.contains(remaining.head)) {
              command += remaining.head
              remaining = remaining.tail
            }
            command += remaining.head
            remaining = remaining.tail
            result = result + command
            mustReset = command != Style.Codes.reset
          } else {
            result += ch
            counter += 1
            remaining = remaining.tail
          }
        }

        if mustReset then result + Style.Codes.reset
        else result

  extension (s: String)
    def printable: String = {
      s.replaceAllLiterally(Sequences.ESC + "", "^")
    }
