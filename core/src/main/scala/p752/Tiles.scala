package p752

object Tiles:
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

  extension (content: String)
    def render(style: Style): String = style.render(content)

  extension (content: String)
    def horizontalFit(
        w: Int,
        vh: Align.Horizontal = Align.Horizontal.Center
    ): String = {
      content.horizontalAlign(empty(w, 1), vh)
    }

  extension (content: String)
    def horizontalAlign(
        behind: String,
        vh: Align.Horizontal = Align.Horizontal.Center
    ): String = {
      if content.indexOf('\n') >= 0 then
        throw new Exception("Expected single line")
      val bw = behind.pureSize
      val w = content.pureSize
      val fitBehind = if w > bw then behind + empty(bw - w, 1) else behind
      val dw = fitBehind.pureSize - w
      val (left, right) = vh match {
        case Align.Horizontal.Right =>
          dw -> 0
        case Align.Horizontal.Center =>
          (dw / 2) -> (dw - (dw / 2))
        case Align.Horizontal.Left =>
          0 -> dw
      }
      fitBehind.pureTake(left) + content + fitBehind.pureTakeLast(right)
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
        case Align.Horizontal.Right =>
          dw -> 0
        case Align.Horizontal.Center =>
          (dw / 2) -> (dw - (dw / 2))
        case Align.Horizontal.Left =>
          0 -> dw
      }

      val above = va match {
        case Align.Vertical.Bottom =>
          dh
        case Align.Vertical.Center =>
          dh / 2
        case Align.Vertical.Top =>
          0
      }

      var behindLines = fitBehind.toLines

      val topPart = behindLines.take(above)
      behindLines = behindLines.drop(above)
      val bottomPart = behindLines.drop(mh)

      val middle = mainContent.square.toLines.zip(behindLines.take(mh)).map {
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
        else counter += 1
        remaining = remaining.tail

      }
      counter

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
      s.replaceAll(Sequences.ESC + "", "^")
    }

  def renderVertical(items: String*): String =
    items.mkString("\n")

  def renderHorizontal(items: String*): String =
    val h = items.map(_.toLines.length).max
    val squaredItems = items.map { item =>
      val w = item.toLines.map(_.pureSize).max
      item.fillSquare(w, h)
    }
    val all = squaredItems.toList.map(_.toLines)
    (0 until h)
      .map(index => all.map(_.apply(index)).reduce(_ + _))
      .mkString("\n")

  def renderHorizontal(align: Align.Horizontal, items: String*): String =
    val parts = items.flatMap(_.split("\n"))
    val maxLen = parts.map(_.pureSize).max
    parts.map(_.horizontalFit(maxLen, align)).mkString("\n")

end Tiles
