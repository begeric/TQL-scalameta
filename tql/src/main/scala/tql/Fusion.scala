package tql


/**
 * Created by Eric on 21.12.2014.
 */

import scala.language.higherKinds
import MonoidEnhencer._

trait Fusion[T] { self: Traverser[T] with Combinators[T] =>

  /**
   * Abstract class to allow two strategy to fuse.
   * It is a system-f class in order to allow the fusion to different kind of strategy (down, up..) without
   * having to duplicate the whole thing each time.
   * */
  abstract class Fused[A : Monoid, F[A] <: Fused[A, F]](val m1: Matcher[A]) extends Matcher[A] {
    /**
     * Exists to not have to use reflection with something like f.getClass.getConstructor[..]... inside compose
     * */
    def newInstance[B : Monoid](m: Matcher[B]): F[B]


    /**
     * Note for the following composition override: it is safe to have the @unchecked annotation here since we know that in F[?] ? will be B since:
     * 1) we accept a Matcher[B]
     * 2) F[B] <: Matcher[B]
     *
     * since we override compose we cannot have an implicit of type ClassTag[F[B]]. It would need to be inserted in the
     * argument list of compose.
     * */

    /**
     * strategy(a) + strategy(b) = strategy(a + b)
     * */
    override def compose[B >: A : Monoid](m2: => Matcher[B]): Matcher[B] = m2 match {
      /*case v: MappedFused[_, B, _] @unchecked =>
        new MappedFused(m1 aggregate v.m1, (x: (A, B)) => v.f(x._1) + x._2) */
      case f: F[B] @unchecked => f.newInstance(m1 compose f.m1)
      case _=> super.compose(m2)
    }

    /**
     * strategy(a) +> strategy(b) = strategy(a +> b)
     * */
    override def composeResults[B >: A : Monoid](m2: => Matcher[B]): Matcher[B] = m2 match {
      case f: F[B] @unchecked => f.newInstance(m1 composeResults f.m1)
      case _=> super.composeResults(m2)
    }

    /**
     *  strategy(a) ~ strategy(b) = strategy(a ~ b)
     * */
    override def aggregate[B : Monoid, C >: A : Monoid](m2: => Matcher[B]): Matcher[(C, B)] = m2 match {
      case f: F[B] @unchecked => f.newInstance(m1 aggregate f.m1)
      case _=> super.aggregate(m2)
    }

    /**
     * strategy(x) map {a => b} + strategy(z) = strategy(x ~ z) map{case (a, z) => (b, z)}
     * */
    override def map[B](f: A => B): Matcher[B] = new MappedFused(newInstance(m1), f)

    /**
     * 1) strategy(x) feed (y => strategy(z)) + strategy(w) = strategy(x) feed (y => strategy(z) + strategy(w))
     * 2) strategy(x) feed (y => strategy(z)) + strategy(v) feed (u => strategy(w)) =
     *    (strategy(x) ~ strategy(v)) feed {case (y,u) => strategy(z) + strategy(w)}
     */

  }

  class MappedFused[A : Monoid, +B, F[A] <: Fused[A, F]](val m1: F[A], val f: A => B) extends Matcher[B] {

    def apply(t: T) = for {
      (v, t) <- m1(t)
    } yield ((v, f(t)))

    def leftCompose[U : Monoid, C >: B : Monoid](mf: MappedFused[U, C, F])/*(implicit m: Monoid[B])*/ = {
      val newmf: F[(U, A)]    = (mf.m1 aggregate m1).asInstanceOf[F[(U, A)]] //since mf.m1 : F[U] and m1: F[A] we know it's same. It's still ugly tho
      val newf: ((U, A)) => C = (x: (U, A)) => implicitly[Monoid[C]].append(mf.f(x._1), f(x._2))
      new MappedFused(newmf, newf)
    }

    override def compose[C >: B : Monoid](m2: => Matcher[C]): Matcher[C] = m2 match {
      case v: MappedFused[_, C, F] @unchecked =>
        v.leftCompose(this)//(implicitly[Monoid[A]], implicitly[Monoid[C]].asInstanceOf[Monoid[B]])
      /*case v: F[C] @unchecked =>
        new MappedFused(m1 aggregate v.m1, (x: (A, B)) => f(x._1) + x._2)
      case _=> super.compose(m2)  */
    }

  }



  /**
   * Create a fuser for the TopDown strategy (down)
   * */
  class FusedTopDown[A : Monoid](override val m1: Matcher[A]) extends Fused[A, FusedTopDown](m1) {
    def newInstance[B : Monoid](m: Matcher[B]) = new FusedTopDown[B](m)
    def apply(t: T) = (m1 + children(this)).apply(t)
  }

  override def down[A : Monoid](m: Matcher[A]): Matcher[A] = new FusedTopDown[A](m)

}
