package p752

object Util:
  def changeAt[T](n: Int, in: List[T], f: T => T): List[T] =
    in.take(n) ::: (f(in(n)) :: in.drop(n + 1))
