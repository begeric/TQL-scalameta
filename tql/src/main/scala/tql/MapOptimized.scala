package tql

/**
 * Created by Eric on 21.12.2014.
 */
trait MapOptimized[T, U] { self: Traverser[T] with Combinators[T] =>

  /**
   * TODO doc
   * */
  class MapTagOptimized[+A : Monoid](val elems: Map[U, Matcher[A]]) extends Matcher[A] {

    override def compose[B >: A : Monoid](m2: => Matcher[B]): Matcher[B] = m2 match {
      case f: MapTagOptimized[B] =>
        val newMap = elems.foldLeft(f.elems)((acc, c) => c match {
          case e @ (i, m) =>  if (acc contains i) acc.updated(i, acc(i) + m)
          else acc + e
        })
        new MapTagOptimized[B](newMap)
      case _ => super.compose(m2)
    }

    def apply(t: T) = elems.getOrElse(TtoU(t), (_: T) => None).apply(t)
  }

  def TtoU(t: T): U
}
