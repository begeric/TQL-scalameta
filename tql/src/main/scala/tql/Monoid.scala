package tql

/**
 * Created by Eric on 20.10.2014.
 */

/**
 * https://en.wikipedia.org/wiki/Monoid
 * */
trait Monoid[A]{
  def zero: A
  def append(a: A, b: A): A
}



object Monoid {

  import scala.collection.generic._

  /*implicit def traversableMonoid[A, B[A] <: Traversable[A]] = new Monoid[B[A]] {
    def zero = implicitly[CanBuildFrom[B[A], A, B[A]]].apply().result
    def append(a: B[A], b: B[A]): B[A] = {
      val x = implicitly[CanBuildFrom[B[A], A, B[A]]].apply()
      x ++= a
      x ++= b
      x.result()
    }
  }   */

  //why it is that way http://stackoverflow.com/questions/15623585/why-is-list-a-semigroup-but-seq-is-not
  implicit def listMonoid[A] = new Monoid[List[A]] {
    def zero = Nil
    def append(a: List[A], b: List[A]) = a ::: b
  }

  implicit def seqMonoid[A] = new Monoid[Seq[A]] {
    def zero = Nil
    def append(a: Seq[A], b: Seq[A]) = a ++ b
  }

  implicit def setMonoid[A] = new Monoid[Set[A]] {
    def zero = Set[A]()
    def append(a: Set[A], b: Set[A]) = a ++ b
  }

  implicit def mapMonoid[A, B] = new Monoid[Map[A, B]] {
    def zero = Map[A, B]()
    def append(a: Map[A, B], b: Map[A, B]) = a ++ b
  }

  implicit object Void extends Monoid[Unit]{
    def zero = ()
    def append(a: Unit, b: Unit) = ()
  }

  //TODO create a macro ?
  implicit def tupleMonoid[A : Monoid, B: Monoid] = new Monoid[(A, B)] {
    import MonoidEnhencer._

    def zero = (implicitly[Monoid[A]].zero,  implicitly[Monoid[B]].zero)
    def append(a: (A, B), b: (A, B)) = (a._1 + b._1, a._2 + b._2)
  }
}