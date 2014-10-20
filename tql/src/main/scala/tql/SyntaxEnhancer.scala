package tql

/**
 * Created by Eric on 19.10.2014.
 */
trait SyntaxEnhancer[T] { self: Combinators[T] with Traverser[T] =>

  implicit class TEnhancer(t: T){
    def \[A : Monoid] (b: TreeMapper[T, A]) = children(b)(implicitly[Monoid[A]])(t)
    def \\[A : Monoid](b: TreeMapper[T, A]) = deep(b)(implicitly[Monoid[A]])(t)

  }

  /*Required for things inside TreeMapperEnhancer*/
  def deepAlias[A : Monoid](m: TreeMapper[T, A]) = deep(m)
  def multiAlias[A : Monoid](m: TreeMapper[T, A]) = multi(m)
  def deepestAlias[A : Monoid](m: TreeMapper[T, A]) = deepest(m)

  implicit class TreeMapperEnhancer[A : Monoid](a: TreeMapper[T, A]){
    def \ (b: TreeMapper[T, A]) = a andThen children(b)
    def \\ (b: TreeMapper[T, A]) = a andThen deepAlias(b)
    def >>[U <: T, B] (f: U => MatcherResult[U, B]) = flatMap(f)
    def apply(f: PartialFunction[T, T]) = update(f)

    def collect = a.map(List(_))
    def deep = deepAlias(a)
    def multi = multiAlias(a)
    def deepest = deepestAlias(a)
  }

  implicit class MatcherResultEnhancer[U <: T, B](a: MatcherResult[U, B]){
    def result  = a.map(_._2)
    def tree    = a.map(_._1)
  }

}
