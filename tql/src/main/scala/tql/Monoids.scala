package tql

/**
 * Created by Eric on 20.10.2014.
 */

object Monoids {
  implicit def listMonoid[A] = new Monoid[List[A]] {
    def zero = Nil
    def append(a: List[A], b: List[A]) = a ++ b
  }

  implicit object IntMonoid extends Monoid[Int] {
    def zero = 0
    def append(l: Int, r: Int) = l + r
  }

  implicit object Void extends Monoid[Unit]{
    def zero = ()
    def append(a: Unit, b: Unit) = ()
  }
}