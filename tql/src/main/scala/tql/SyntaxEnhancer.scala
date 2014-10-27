package tql

import scala.reflect.ClassTag

/**
 * Created by Eric on 19.10.2014.
 */
trait SyntaxEnhancer[T] { self: Combinators[T] with Traverser[T] =>
  //Convention : Operators ending with : are right assosiative
  implicit class TEnhancer(t: T){
    def \[A : Monoid] (b: TreeMapper[A]) = children(b)(implicitly[Monoid[A]])(t)
    def \\[A : Monoid](b: TreeMapper[A]) = downBreak(b)(implicitly[Monoid[A]])(t)

    def >>[A: Monoid](a: TreeMapper[A]) = a(t)

  }

  /*Required for things inside TreeMapperEnhancer*/
  def downBreakAlias[A : Monoid](m: TreeMapper[A]) = downBreak(m)
  def downAlias[A : Monoid](m: TreeMapper[A]) = down(m)
  def upBreakAlias[A : Monoid](m: TreeMapper[A]) = upBreak(m)

  implicit class TreeMapperEnhancer[A](a: TreeMapper[A]){
    def \: (b: TreeMapper[A])(implicit x: Monoid[A]) = a andThen children(b)
    def \\[B : Monoid] (b: TreeMapper[B]) = a andThen downBreakAlias(b)
    def >>[B] (f: T => MatcherResult[B]) = flatMap(f)
    def apply[I <: T : ClassTag, O <: T](f: PartialFunction[I, O])(implicit x: ClassTag[T], y: AllowedTransformation[I, O]) = updateE[I, O](f)

    def collect = a.map(List(_))
    def downBreak(implicit x: Monoid[A]) = downBreakAlias(a)
    def down(implicit x: Monoid[A]) = downAlias(a)
    def upBreak(implicit x: Monoid[A]) = upBreakAlias(a)
  }

  implicit class MatcherResultEnhancer[B](a: MatcherResult[B]){
    def result  = a.map(_._2)
    def tree    = a.map(_._1)
  }

}
