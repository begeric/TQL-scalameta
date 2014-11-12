package tql

/**
 * Created by Eric on 19.10.2014.
 */

import scala.reflect.ClassTag

trait SyntaxEnhancer[T] { self: Combinators[T] with Traverser[T] =>
  //Convention : Operators ending with : are right assosiative
  implicit class TEnhancer(t: T){
    def \[A : Monoid] (b: Matcher[A]) = children(b)(implicitly[Monoid[A]])(t)
    def \\[A : Monoid](b: Matcher[A]) = downBreak(b)(implicitly[Monoid[A]])(t)

    def >>[A](a: Matcher[A]) = a(t)

    def resultOf[A : Monoid](a: Matcher[A]) = new MatcherResultEnhancer(a(t)).result
    def treeOf[A : Monoid](a: Matcher[A]) = new MatcherResultEnhancer(a(t)).tree

  }

  /*Required for things inside TreeMapperEnhancer*/
  def downBreakAlias[A : Monoid](m: Matcher[A]) = downBreak(m)
  def downAlias[A : Monoid](m: Matcher[A]) = down(m)
  def upBreakAlias[A : Monoid](m: Matcher[A]) = upBreak(m)

  implicit class TreeMapperEnhancer[A](a: Matcher[A]){
    def \[B : Monoid] (b: Matcher[B]) = a andThen children(b)
    def \\[B : Monoid] (b: Matcher[B]) = a andThen downBreakAlias(b)
    def >>[B] (f: T => MatcherResult[B]) = flatMap(f)
    def apply[I <: T : ClassTag, O <: T]
             (f: PartialFunction[I, O])
             (implicit x: ClassTag[T], y: AllowedTransformation[I, O]) =
      transform[I, O](f)

    def collect = a.map(List(_))
    def downBreak(implicit x: Monoid[A]) = downBreakAlias(a)
    def down(implicit x: Monoid[A]) = downAlias(a)
    def upBreak(implicit x: Monoid[A]) = upBreakAlias(a)
  }

  implicit class MatcherResultEnhancer[B](a: MatcherResult[B]){
    def result(implicit x: Monoid[B])  = a.map(_._2).getOrElse(x.zero)
    def tree    = a.map(_._1)
  }

  implicit def MatcherResultToResult[B : Monoid](a: MatcherResult[B]): B = a.result
  implicit def MatcherResultToTree(a : MatcherResult[_]): Option[T] = a.tree

}
