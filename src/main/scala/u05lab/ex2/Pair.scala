package u05lab.ex2

trait Pair[A, B] {
  def x : A
  def y : B
}

private case class PairImpl[A, B](override val x : A, override val y : B) extends Pair[A, B]

object Pair {
  def apply[A, B](x : A, y : B): Pair[A, B] =
    PairImpl(x, y)
}
