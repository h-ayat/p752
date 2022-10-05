package cons

import scala.scalanative.unsafe.{extern, link}

@extern
object NativeBindings {
  def nextChar(): Int = extern

  def init(): Unit = extern

  def end(): Unit = extern
}
