package tql

import scala.collection.GenSeq
import scala.collection.generic.CanBuildFrom

/**
 * Created by Eric on 20.10.2014.
 */

object Monoids {

  //why it is that way http://stackoverflow.com/questions/15623585/why-is-list-a-semigroup-but-seq-is-not
  implicit def listMonoid[A] = new Monoid[List[A]] {
    def zero = Nil
    def append(a: List[A], b: List[A]) = a ::: b
  }

  implicit object Void extends Monoid[Unit]{
    def zero = ()
    def append(a: Unit, b: Unit) = ()
  }

  implicit def tupleMonoid[A : Monoid, B: Monoid] = new Monoid[(A, B)] {
    import MonoidEnhencer._

    def zero = (implicitly[Monoid[A]].zero,  implicitly[Monoid[B]].zero)
    def append(a: (A, B), b: (A, B)) = (a._1 + b._1, a._2 + b._2)
  }
}