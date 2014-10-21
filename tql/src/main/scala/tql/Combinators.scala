package tql

trait Combinators[T] { self: Traverser[T] =>

  import scala.reflect.{ClassTag, classTag}

  def children[U <: T : ClassTag, V <: T, A : Monoid](f: TreeMapper[U, V, A]) =  TreeMapper[U, U, A]{ tree =>
    traverse(tree, f)
  }

  def deep[U <: T : ClassTag, V >: U <: T, A : Monoid](m: TreeMapper[U, V, A]): TreeMapper[U, V, A] =
    m | children(deep[U, V, A](m))

  def deepest[U <: T : ClassTag, V >: U <: T, A : Monoid](m: TreeMapper[U, V, A]): TreeMapper[U, V, A] =
    children(deep[U, V, A](m)) | m

  def multi[U <: T : ClassTag, V <: U : ClassTag, A : Monoid](m: TreeMapper[U, V, A]): TreeMapper[U, V, A] =
    m + children(multi[U, V, A](m))

  def flatMap[U <: T : ClassTag, V <: T, B](f: U => MatcherResult[V, B]) = TreeMapper[U, V, B] {tree =>
    f(tree)
  }  /*
  def filter(f: T => Boolean) = TreeMapper[T, T]{ t =>
    if (f(t))  Some((t, t))
    else None
  }
  def filterPartial[U <: T](f: PartialFunction[U, Boolean]) = TreeMapper[T, U]{
    case t: U if f.isDefinedAt(t) && f(t) => Some((t, t))
    case _ => None
  }

  def collect[U <: T, A](f: PartialFunction[U, A]) = filterPartial[U]{case t => f.isDefinedAt(t)} map (x => List(f(x)))*/

  def filter[U <: T : ClassTag](f: PartialFunction[U, Boolean]) = TreeMapper[U, U, U]{
    case t: U if f.isDefinedAt(t) && f(t) => Some((t, t))
    case _ => None
  }

  trait AllowedTransformation[I, O]

  def update[I <: T : ClassTag, O <: T](f: PartialFunction[I, O])(implicit x: AllowedTransformation[I, O]) = TreeMapper[I, O, Unit] { tree =>
    if (f.isDefinedAt(tree) ) Some((f(tree), Monoids.Void.zero))
    else None
  }

}