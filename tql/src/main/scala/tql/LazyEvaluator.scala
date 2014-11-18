package tql
/**
 * Created by Eric on 16.11.2014.
 */

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

/**
 * This trait allows to easily write simple traversals.
 * Instead of writing:
 *  t : T
 *  val x = down(collect{...})
 *  val result = x(t).result
 * We can wirte instead
 *  t.collect{...}.result
 *
 *  downBreak(filter{..} ~> down{update{...}}) (t)
 *  becomes
 *  t.downBreak.filter{..}.down.update{..}
 *  Which is essentially easier to read and to write (no parenthesis)
 *  The drawbacks:
 *    - Every combinator have to be re-written in those 'Lazy evaluator' classes (even macros)
 *    - Composition is lost.
 *
 *  Of course everything is computed lazily (like Collection View in Scala) so the resulte have to be forced.
 * */
trait LazyEvaluator[T] { self: Combinators[T] with Traverser[T] with SyntaxEnhancer[T] =>

  import scala.language.experimental.macros

  /**Abstract class used to delay delay the time when the type parameter of
    * a meta combinator is decided*/
  abstract class DelayedMeta{
    def apply[A : Monoid](m: Matcher[A]): Matcher[A]
  }

  /**
   * Allows to call 'combinators' directly on T
   * For documentation see Combinators.scala
   * */
  implicit class LazyEvaluator(t: T){

    def collect[A](f: PartialFunction[T, A])(implicit x: ClassTag[T], y : ClassTag[A]) =
      down.collect(f)

    def collectIn[C[_]] = new {
      def apply[A](f: PartialFunction[T, A])(implicit x: ClassTag[T], y: CanBuildFrom[C[A], A, C[A]])  =
        down.collectIn[C](f)
    }

    def guard[U <: T : ClassTag](f: PartialFunction[U, Boolean]) = down.guard(f)

    def filter(f: PartialFunction[T, Boolean]): LazyEvaluatorAndThen[T] = macro CombinatorsSugar.filterSugarImpl[T]

    def transform[I <: T : ClassTag, O <: T](f: PartialFunction[I, O])(implicit x: AllowedTransformation[I, O]) =
      down.transform(f)

    def update(f: PartialFunction[T, T]): LazyEvaluatorAndThen[Unit] = macro CombinatorsSugar.updateSugarImpl[T]

    def visit[A](f: PartialFunction[T, A])(implicit x: ClassTag[T]) = down.visit(f)

    def flatMap[B](f: T => MatcherResult[B]) = down.flatMap(f)

    def down      = new LazyEvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = self.down(x)})
    def downBreak = new LazyEvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = self.downBreak(x)})
    def up        = new LazyEvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = self.up(x)})
    def upBreak   = new LazyEvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = self.upBreak(x)})
    def children  = new LazyEvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = self.children(x)})
  }

  class LazyEvaluatorMeta(t: T, meta: DelayedMeta){

    def collect[A](f: PartialFunction[T, A])(implicit x: ClassTag[T], y : ClassTag[A]) =
      new LazyEvaluatorAndThen(t, self.collect[A](f), meta)

    def collectIn[C[_]] = new {
      def apply[A](f: PartialFunction[T, A])(implicit x: ClassTag[T], y: CanBuildFrom[C[A], A, C[A]])  =
        new LazyEvaluatorAndThen(t, self.collectIn[C](f), meta)
    }

    def guard[U <: T : ClassTag](f: PartialFunction[U, Boolean]) =
      new LazyEvaluatorAndThen(t, self.guard(f), meta)

    def filter(f: PartialFunction[T, Boolean]): LazyEvaluatorAndThen[T] =
      macro CombinatorsSugar.filterSugarImpl[T]

    def transform[I <: T : ClassTag, O <: T](f: PartialFunction[I, O])(implicit x: AllowedTransformation[I, O]) =
      new LazyEvaluatorAndThen(t, self.transform(f), meta)

    def update(f: PartialFunction[T, T]): LazyEvaluatorAndThen[Unit] =
      macro CombinatorsSugar.updateSugarImpl[T]

    def visit[A](f: PartialFunction[T, A])(implicit x: ClassTag[T]) =
      new LazyEvaluatorAndThen[A](t, self.visit(f), meta)

    def flatMap[B](f: T => MatcherResult[B]) = new LazyEvaluatorAndThen[B](t, self.flatMap(f), meta)

    /**
     * Allows to use other combinators which are not defined in the LazyEvaluator framework
     * */
    def combine[B](x: Matcher[B]) = new LazyEvaluatorAndThen[B](t, x, meta)
  }



  class LazyEvaluatorAndThen[+A](private[LazyEvaluator] val t: T,
                                 private[LazyEvaluator] val m: Matcher[A],
                                 private[LazyEvaluator] val meta: DelayedMeta){

    def map[B](f: A => B) = new LazyEvaluatorAndThen[B](t, m map f, meta)

    def collect[B >: A](f: PartialFunction[T, B])(implicit x: ClassTag[T], z : ClassTag[B]) =
      new LazyEvaluatorAndThen(t, m ~> self.collect(f), meta)

    def collectIn[C[_]] = new {
      def apply[A](f: PartialFunction[T, A])(implicit x: ClassTag[T], y: CanBuildFrom[C[A], A, C[A]])  =
        new LazyEvaluatorAndThen(t, m ~> self.collectIn[C](f), meta)
    }

    def guard[U <: T : ClassTag](f: PartialFunction[U, Boolean]) =
      new LazyEvaluatorAndThen(t, m ~> self.guard(f), meta)

    def filter(f: PartialFunction[T, Boolean]): LazyEvaluatorAndThen[T] =
      macro CombinatorsSugar.filterSugarImpl[T]

    def transform[I <: T : ClassTag, O <: T](f: PartialFunction[I, O])(implicit x: AllowedTransformation[I, O]) =
      new LazyEvaluatorAndThen(t, m ~> self.transform(f), meta)

    def update(f: PartialFunction[T, T]): LazyEvaluatorAndThen[Unit] =
      macro CombinatorsSugar.updateSugarImpl[T]

    def visit[A](f: PartialFunction[T, A])(implicit x: ClassTag[T]) =
      new LazyEvaluatorAndThen[A](t, m ~> self.visit(f), meta)

    def combine[B](x: Matcher[B]) = new LazyEvaluatorAndThen[B](t, m ~> x, meta)

    def flatMap[B](f: T => MatcherResult[B]) = new LazyEvaluatorAndThen[B](t, m ~> self.flatMap(f), meta)

    def down =
      new LazyEvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = meta(m ~> self.down(x))})
    def downBreak =
      new LazyEvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = meta(m ~> self.downBreak(x))})
    def up =
      new LazyEvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = meta(m ~> self.up(x))})
    def upBreak =
      new LazyEvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = meta(m ~> self.upBreak(x))})
    def children =
      new LazyEvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = meta(m ~> self.children(x))})
  }

  /**
   * This has to be outside of LazyEvaluatorAndThen because of covarience stuff it is not possible to write
   * def force(implicit x: Monoid[A]) = ...inside LazyEvaluatorAndThen[A]
   * We should write def force[B >: A](implicit x: Monoid[B]) but Monoid should be made contravarient in A,
   * which is not possible (in part because it is not logical and because contravarient stuff does not work well
   * with implicits)
   * */
  implicit class ForceResult[A : Monoid](x : LazyEvaluatorAndThen[A]){
    def force = x.meta(x.m).apply(x.t)
    def result = force.result
    def tree = force.tree
  }


}
