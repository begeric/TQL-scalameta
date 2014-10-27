package tql

/**
 * Created by Eric on 20.10.2014.
 */

object Monoids {

  implicit def listMonoid[A] = new Monoid[List[A]] {
    def zero = Nil
    def append(a: List[A], b: List[A]) = a ++ b
  }

  /*implicit object counterMonoid extends Monoid[Int] {
    def zero = 0
    def append(l: Int, r: Int) = l + r
  }*/

  trait Stateful[+A]
  case class State[A](v: A) extends Stateful[A]
  case object EmptyState extends Stateful[Nothing]

  implicit def stateMonoid[A] = new Monoid[Stateful[A]] {
    def zero = EmptyState
    def append(a: Stateful[A], b: Stateful[A]) = b
  }

  implicit object Void extends Monoid[Unit]{
    def zero = ()
    def append(a: Unit, b: Unit) = ()
  }
}