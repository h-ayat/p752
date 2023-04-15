package p752.tiles

import p752.Prop

object GenericList:

  final case class State[T](items: List[T], index: Int = 0) {
    def inc(items: Int): State[T] =
      if index < items - 1 then copy(index = index + 1) else copy(index = 0)
    def dec(items: Int): State[T] =
      if index > 0 then copy(index = index - 1) else copy(index = items - 1)

    lazy val selected: Option[T] =
      if index >= 0 && index < items.length then Some(items(index)) else None

  }

  final case class Style(
      default: Prop = Prop.empty,
      selected: Prop = Prop(255, 197, bold = true)
  )

  val defaultStyle = Style()

  final case class ItemSelected[T](value: T)
