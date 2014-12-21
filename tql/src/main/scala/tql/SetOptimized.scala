package tql

/**
 * Created by Eric on 21.12.2014.
 */
trait SetOptimized[T, U] { self: Traverser[T] with Combinators[T] =>

  /**
   * TODO doc
   * */
  class SetOptimized[+A](val elems: Set[U], val m1: Matcher[A]) extends Matcher[A] {

    override def compose[B >: A : Monoid](m2: => Matcher[B]): Matcher[B] = m2 match {
      case f: SetOptimized[B] => new SetOptimized[B](elems ++ f.elems, m1 + f.m1)
      case _ => super.compose(m2)
    }

    def apply(t: T) =
      if(elems contains TtoU(t))
        m1(t)
      else
        None
  }

  def TtoU(t: T): U
}
