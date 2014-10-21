package tql

import scala.reflect.ClassTag

/**
 * Created by Eric on 19.10.2014.
 */
trait SyntaxEnhancer[T] { self: Combinators[T] with Traverser[T] =>

  implicit class TEnhancer(t: T){
    def \[A : Monoid] (b: TreeMapper[A]) = children(b)(implicitly[Monoid[A]])(t)
    def \\[A : Monoid](b: TreeMapper[A]) = deep(b)(implicitly[Monoid[A]])(t)

  }

  /*Required for things inside TreeMapperEnhancer*/
  def deepAlias[A : Monoid](m: TreeMapper[A]) = deep(m)
  def multiAlias[A : Monoid](m: TreeMapper[A]) = multi(m)
  def deepestAlias[A : Monoid](m: TreeMapper[A]) = deepest(m)

  implicit class TreeMapperEnhancer[A](a: TreeMapper[A]){
    def \ (b: TreeMapper[A])(implicit x: Monoid[A]) = a andThen children(b)
    def \\[B: Monoid] (b: TreeMapper[B]) = a andThen deepAlias(b)
    def >>[B] (f: T => MatcherResult[B]) = flatMap(f)
    def apply[I <: T : ClassTag, O <: T](f: PartialFunction[I, O])(implicit x: ClassTag[T], y: AllowedTransformation[I, O]) = update(f)

    def collect = a.map(List(_))
    def deep(implicit x: Monoid[A]) = deepAlias(a)
    def multi(implicit x: Monoid[A]) = multiAlias(a)
    def deepest(implicit x: Monoid[A]) = deepestAlias(a)
  }

  implicit class MatcherResultEnhancer[B](a: MatcherResult[B]){
    def result  = a.map(_._2)
    def tree    = a.map(_._1)
  }

}
