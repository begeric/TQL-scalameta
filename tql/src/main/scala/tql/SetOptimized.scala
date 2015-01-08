package tql

/**
 * Created by Eric on 21.12.2014.
 */
trait SetOptimized[T, U] { self: Traverser[T] with Combinators[T] =>

  /**
   * The idea of this optimization is to 'apply' the Matcher (m1) only when a property on the data structure T
   * of type U is contained in the Set[U] elems.
   *
   * For example if you have:
   * t1 = optimize{case Function.. => //do stuff1}
   * t2 = optimize{case Int(..) => //do stuff2}
   * t3 = optimize{case Function => //do stuff3}
   * t4 = optimize{case Function => // //do stuff4}
   * And you combine them together, then for each tree that you traverse you have to try to call 4 partial functions.
   * But with SetOptimized a the Matcher's are applied only if the tree is of type Function or Int i.e.
   *   traverse(tree: Tree) {
   *       if (tree.isInstanceOf[Function] || tree.isInstanceOf[Int]) {
   *           t1(tree)
   *           t2(tree)
   *           t3(tree)
   *           t4(tree)
   *       }
   *   }
   * */
  class SetOptimized[+A](val elems: Set[U], val m1: Matcher[A]) extends Matcher[A] {

    override def aggregate[B >: A : Monoid](m2: => Matcher[B]): Matcher[B] = m2 match {
      case f: SetOptimized[B] => new SetOptimized[B](elems ++ f.elems, m1 + f.m1)
      case _ => super.aggregate(m2)
    }

    def apply(t: T) =
      if(elems contains TtoU(t))
        m1(t)
      else
        None
  }

  def TtoU(t: T): U
}
