package tql

/**
 * Created by Eric on 21.12.2014.
 */
trait MapOptimized[T, U] { self: Traverser[T] with Combinators[T] =>

  /**
   * The idea is the same as in @see SetOptimized, but this time the Matcher's are grouped by the properties they share.
   * Instead you make it so that the 3 pfs that pattern match against Function are called only if
   * the tree that you traverse is of type Function i.e.
   *  traverse(tree: Tree) {
   *       if (tree.isInstanceOf[Function] || tree.isInstanceOf[Int]) {
   *           t1(tree)
   *           t2(tree)
   *           t3(tree)
   *           t4(tree)
   *       }
   *       else if (tree.isInstanceOf[Int]) {
   *           t2(tree)
   *        }
   *   }
   * */
  class MapTagOptimized[A : Monoid](val elems: Map[U, Matcher[A]]) extends Matcher[A] {

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
