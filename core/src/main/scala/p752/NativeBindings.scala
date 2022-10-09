package p752

import scala.scalanative.unsafe.extern

@extern
object NativeBindings {
  def nextChar(): Int = extern

  def init(): Unit = extern

  def end(): Unit = extern
}
