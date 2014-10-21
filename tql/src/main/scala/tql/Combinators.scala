package tql

import scala.reflect.ClassTag

trait Combinators[T] { self: Traverser[T] =>

  import scala.reflect.{ClassTag, classTag}

  def children[A : Monoid](f: TreeMapper[A]) =  TreeMapper[A]{ tree =>
    traverse(tree, f)
  }

  def deep[A : Monoid](m: TreeMapper[A]): TreeMapper[A] =
    m | children(deep[A](m))

  def deepest[A : Monoid](m: TreeMapper[A]): TreeMapper[A] =
    children(deep[A](m)) | m

  def multi[A : Monoid](m: TreeMapper[A]): TreeMapper[A] =
    m + children(multi[A](m))

  def flatMap[B](f: T => MatcherResult[B]) = TreeMapper[B] {tree =>
    f(tree)
  }

  def collect[A](f: PartialFunction[T, A])(implicit x: ClassTag[T]) = filter[T]{case t => f.isDefinedAt(t)} map (x => List(f(x)))

  def filter[U <: T : ClassTag](f: PartialFunction[U, Boolean]) = TreeMapper[U]{
    case t: U if f.isDefinedAt(t) && f(t) => Some((t, t))
    case _ => None
  }

  trait AllowedTransformation[I, O]

  def update[I <: T : ClassTag, O <: T](f: PartialFunction[I, O])(implicit x: AllowedTransformation[I, O]) = TreeMapper[Unit] {
    case t: I if (f.isDefinedAt(t)) => Some((f(t), Monoids.Void.zero))
    case _ => None
  }

}