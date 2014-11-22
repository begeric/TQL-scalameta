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
trait CollectionLikeUI[T] { self: Combinators[T] with Traverser[T] with SyntaxEnhancer[T] =>

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
  implicit class Evaluator(t: T){

    def collect[A](f: PartialFunction[T, A])(implicit x: ClassTag[T], y : ClassTag[A]) =
      down.collect(f)

    def collectIn[C[_]] = new {
      def apply[A](f: PartialFunction[T, A])(implicit x: ClassTag[T], y: CanBuildFrom[C[A], A, C[A]], z: Monoid[C[A]])  =
        down.collectIn[C](f)
    }

    def guard[U <: T : ClassTag](f: PartialFunction[U, Boolean]) = down.guard(f)

    def filter(f: PartialFunction[T, Boolean]): EvaluatorAndThen[T] = macro CombinatorsSugar.filterSugarImpl[T]

    def transform[I <: T : ClassTag, O <: T](f: PartialFunction[I, O])(implicit x: AllowedTransformation[I, O]) =
      down.transform(f)

    def update(f: PartialFunction[T, T]): Option[T] = macro CombinatorsSugar.updateSugarImpl[T]

    def visit[A : Monoid](f: PartialFunction[T, A])(implicit x: ClassTag[T]) = down.visit(f)

    def flatMap[B](f: T => MatcherResult[B]) = down.flatMap(f)

    def down      = new EvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = self.down(x)})
    def downBreak = new EvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = self.downBreak(x)})
    def up        = new EvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = self.up(x)})
    def upBreak   = new EvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = self.upBreak(x)})
    def children  = new EvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = self.children(x)})
  }

  class EvaluatorMeta(t: T, meta: DelayedMeta){

    def collect[A](f: PartialFunction[T, A])(implicit x: ClassTag[T], y : ClassTag[A]) =
      meta(self.collect[A](f)).apply(t).result

    def collectIn[C[_]] = new {
      def apply[A](f: PartialFunction[T, A])(implicit x: ClassTag[T], y: CanBuildFrom[C[A], A, C[A]], z: Monoid[C[A]])  =
        meta(self.collectIn[C](f)).apply(t).result
    }

    def guard[U <: T : ClassTag](f: PartialFunction[U, Boolean]) =
      new EvaluatorAndThen(t, self.guard(f), meta)

    def filter(f: PartialFunction[T, Boolean]): EvaluatorAndThen[T] =
      macro CombinatorsSugar.filterSugarImpl[T]

    def transform[I <: T : ClassTag, O <: T](f: PartialFunction[I, O])(implicit x: AllowedTransformation[I, O]) =
      meta(self.transform(f)).apply(t).tree

    def update(f: PartialFunction[T, T]): Option[T] =
      macro CombinatorsSugar.updateSugarImpl[T]

    def visit[A : Monoid](f: PartialFunction[T, A])(implicit x: ClassTag[T]) =
      meta(self.visit(f)).apply(t)


    /**
     * Allows to use other combinators which are not defined in the CollectionLikeUI framework
     * */
    def combine[B](x: Matcher[B]) = new EvaluatorAndThen[B](t, x, meta)

    def flatMap[B](f: T => MatcherResult[B]) = new EvaluatorAndThen[B](t, self.flatMap(f), meta)
  }


  class EvaluatorAndThen[+A](private[CollectionLikeUI] val t: T,
                                 private[CollectionLikeUI] val m: Matcher[A],
                                 private[CollectionLikeUI] val meta: DelayedMeta){

    def map[B](f: A => B) = new EvaluatorAndThen[B](t, m map f, meta)

    def collect[B >: A](f: PartialFunction[T, B])(implicit x: ClassTag[T], z : ClassTag[B]) =
      meta(m ~> self.collect(f)).apply(t).result

    def collectIn[C[_]] = new {
      def apply[A](f: PartialFunction[T, A])(implicit x: ClassTag[T], y: CanBuildFrom[C[A], A, C[A]], z: Monoid[C[A]])  =
        meta(m ~> self.collectIn[C](f)).apply(t).result
    }

    def guard[U <: T : ClassTag](f: PartialFunction[U, Boolean]) =
      new EvaluatorAndThen(t, m ~> self.guard(f), meta)

    def filter(f: PartialFunction[T, Boolean]): EvaluatorAndThen[T] =
      macro CombinatorsSugar.filterSugarImpl[T]

    def transform[I <: T : ClassTag, O <: T](f: PartialFunction[I, O])(implicit x: AllowedTransformation[I, O]) =
      meta(m ~> self.transform(f)).apply(t).tree

    def update(f: PartialFunction[T, T]): Option[T] =
      macro CombinatorsSugar.updateSugarImpl[T]

    def visit[A : Monoid](f: PartialFunction[T, A])(implicit x: ClassTag[T]) =
      meta(m ~> self.visit(f)).apply(t)

    def combine[B](x: Matcher[B]) = new EvaluatorAndThen[B](t, m ~> x, meta)

    def flatMap[B](f: T => MatcherResult[B]) = new EvaluatorAndThen[B](t, m ~> self.flatMap(f), meta)

    def down =
      new EvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = meta(m ~> self.down(x))})
    def downBreak =
      new EvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = meta(m ~> self.downBreak(x))})
    def up =
      new EvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = meta(m ~> self.up(x))})
    def upBreak =
      new EvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = meta(m ~> self.upBreak(x))})
    def children =
      new EvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = meta(m ~> self.children(x))})
  }

  /**
   * This has to be outside of EvaluatorAndThen because of covarience stuff it is not possible to write
   * def force(implicit x: Monoid[A]) = ...inside EvaluatorAndThen[A]
   * We should write def force[B >: A](implicit x: Monoid[B]) but Monoid should be made contravarient in A,
   * which is not possible (in part because it is not logical and because contravarient stuff does not work well
   * with implicits)
   * */
  implicit class ForceResult[A : Monoid](x : EvaluatorAndThen[A]){
    def force = x.meta(x.m).apply(x.t)
    def result = force.result
    def tree = force.tree
  }
}
