package p752.demo.demos

sealed trait DemoEvent
object DemoEvent {
  case object Ended extends DemoEvent
  case object Pass extends DemoEvent
}
