package tql

/**
 * Created by Eric on 16.11.2014.
 */

import scala.language.higherKinds
import scala.language.reflectiveCalls
import scala.language.experimental.macros

import scala.reflect.ClassTag
import NotEquivTypes._

/**
 * This trait allows to easily write simple traversals.
 * Instead of writing:
 *  t : T
 *  val x = down(collect{...})
 *  val result = x(t).result
 * We can wirte instead
 *  t.collect{...}.result
 *
 *  downBreak(focus{..} ~> down{update{...}}) (t)
 *  becomes
 *  t.downBreak.focus{..}.down.update{..}
 *  Which is essentially easier to read and to write (no parenthesis)
 *  The drawbacks:
 *    - Every combinator have to be re-written in those 'Lazy evaluator' classes (even macros)
 *    - Composition is lost.
 *
 *  Of course everything is computed lazily (like Collection View in Scala) so the resulte have to be forced.
 * */
trait CollectionLikeUI[T] { self: Combinators[T] with Traverser[T] with SyntaxEnhancer[T] =>

  /**Abstract class used to delay delay the time when the type parameter of
    * a meta combinator is decided*/
  abstract class DelayedMeta{
    def apply[A : Monoid](m: Matcher[A]): Matcher[A]
  }

  /**
   * System needed to recover the correct type from a 'transfrom' call.
   * 1) x.transform{case x: T => x} : T
   * 2) x.transform{case x: T => (x, List(1))} : (T, List[Int])
   * */
  trait TransformResultTr[A]{
    type R
    def get(t: T, x: MatchResult[A]): R
  }

  object TransformResultTr{
    //for 1) the case where the returned type is Unit
    implicit val unitRes = new TransformResultTr[Unit] {
      type R = T
      def get(t: T, x: MatchResult[Unit]): R  = x.tree.getOrElse(t)
    }

    //for 2) the case where the returned type is not Unit
    implicit def withRes[A: Monoid](implicit ev: A =!= Unit) = new TransformResultTr[A] {
      type R = (T, A)
      def get(t: T, x: MatchResult[A]): R  = (x.tree.getOrElse(t), x.result)
    }
  }


  /**
   * Allows to call 'combinators' directly on T
   * For documentation see Combinators.scala
   * */
  implicit class Evaluator(t: T){

    def collect[C[_]] = new  {
     def apply[A, R](f: PartialFunction[T, A])(implicit x: ClassTag[T], y: Collector[C[A], A, R], z: Monoid[R]) =
       down.collect[C](f)
    }

    def guard[U <: T : ClassTag](f: PartialFunction[U, Boolean]) = down.guard(f)

    def focus(f: PartialFunction[T, Boolean]): EvaluatorAndThen[T] = macro CombinatorsSugar.filterSugarImpl[T]

    /**
    * This is required for the transform macro as it cannot access self.transformWithResult by itself
    * */
    def transformWithResult[I <: T : ClassTag, O <: T, A]
                        (f: PartialFunction[I, (O, A)])
                        (implicit x: AllowedTransformation[I, O]) = self.transformWithResult(f)

    def transform(f: PartialFunction[T, Any]): Any =
    macro CombinatorsSugar.transformSugarImplWithTRtype[T]

    def transforms[A : Monoid](f: Matcher[A])(implicit r: TransformResultTr[A]) = down.transforms(f)

    def down      = new EvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = self.down(x)})
    def downBreak = new EvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = self.downBreak(x)})
    def up        = new EvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = self.up(x)})
    def upBreak   = new EvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = self.upBreak(x)})
    def children  = new EvaluatorMeta(t, new DelayedMeta{def apply[A : Monoid](x: Matcher[A]) = self.children(x)})
  }

  /**
   * Evaluator at which will be applied the traversal strategy defined in 'meta'
   * */
  class EvaluatorMeta(t: T, meta: DelayedMeta){

    def collect[C[_]] = new {
      def apply[A, R](f: PartialFunction[T, A])(implicit x: ClassTag[T], y: Collector[C[A], A, R], z: Monoid[R]) =
        meta(self.collect[C](f)).apply(t).result
    }

    def guard[U <: T : ClassTag](f: PartialFunction[U, Boolean]) =
      new EvaluatorAndThen(t, self.guard(f), meta)

    def focus(f: PartialFunction[T, Boolean]): EvaluatorAndThen[T] =
      macro CombinatorsSugar.filterSugarImpl[T]

    /**
     * This is required for the transform macro as it cannot access self.transformWithResult by itself
     * */
    def transformWithResult[I <: T : ClassTag, O <: T, A]
                          (f: PartialFunction[I, (O, A)])
                          (implicit x: AllowedTransformation[I, O]) = self.transformWithResult(f)

    def transforms[A : Monoid](f: Matcher[A])(implicit r: TransformResultTr[A]) =
      r.get(t, meta(f).apply(t))

    def transform(f: PartialFunction[T, Any]): Any =
      macro CombinatorsSugar.transformSugarImplWithTRtype[T]

    def visit[A : Monoid](f: PartialFunction[T, A])(implicit x: ClassTag[T]) =
      meta(self.visit(f)).apply(t)


    /**
     * Allows to use other combinators which are not defined in the CollectionLikeUI framework
     * */
    def combine[B](x: Matcher[B]) = new EvaluatorAndThen[B](t, x, meta)
  }


  class EvaluatorAndThen[+A](   private[CollectionLikeUI] val t: T,
                                 private[CollectionLikeUI] val m: Matcher[A],
                                 private[CollectionLikeUI] val meta: DelayedMeta){
    def collect[C[_]] = new {
      def apply[A, R](f: PartialFunction[T, A])(implicit x: ClassTag[T], y: Collector[C[A], A, R], z: Monoid[R])  =
        meta(m ~> self.collect[C](f)).apply(t).result
    }

    def guard[U <: T : ClassTag](f: PartialFunction[U, Boolean]) =
      new EvaluatorAndThen(t, m ~> self.guard(f), meta)

    def focus(f: PartialFunction[T, Boolean]): EvaluatorAndThen[T] =
      macro CombinatorsSugar.filterSugarImpl[T]

    /**
     * This is required for the transform macro as it cannot access self.transformWithResult by itself
     * */
    def transformWithResult[I <: T : ClassTag, O <: T, A]
                            (f: PartialFunction[I, (O, A)])
                            (implicit x: AllowedTransformation[I, O]) = self.transformWithResult(f)

    def transforms[A : Monoid](f: Matcher[A])(implicit r: TransformResultTr[A]) =
      r.get(t, meta(m ~> f).apply(t))

    def transform(f: PartialFunction[T, Any]): Any =
      macro CombinatorsSugar.transformSugarImplWithTRtype[T]

    def combine[B](x: Matcher[B]) = new EvaluatorAndThen[B](t, m ~> x, meta)

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
    def tree = force.tree//.getOrElse(x.t) ??
  }
}
