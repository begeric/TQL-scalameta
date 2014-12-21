package tql

import scala.language.higherKinds
import scala.reflect._

/**
 * Created by Eric on 21.12.2014.
 */
trait Fusion[T] { self: Traverser[T] with Combinators[T] =>

  class FusedTopDown[+A : Monoid](val m1: Matcher[A]) extends Matcher[A] {

    override def compose[B >: A : Monoid](m2: => Matcher[B]): Matcher[B] = m2 match {
      case f: FusedTopDown[B] => new FusedTopDown[B](m1 compose f.m1)
      case _=> super.compose(m2)
    }

    override def composeResults[B >: A : Monoid](m2: => Matcher[B]): Matcher[B] = m2 match {
      case f: FusedTopDown[B] => new FusedTopDown[B](m1 composeResults f.m1)
      case _=> super.composeResults(m2)
    }

    def apply(t: T) = (m1 + children(this)).apply(t)
  }

  override def down[A : Monoid](m: Matcher[A]): Matcher[A] = new FusedTopDown[A](m)
}
