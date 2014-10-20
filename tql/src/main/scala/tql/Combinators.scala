package tql

trait Combinators[T] { self: Traverser[T] =>

  def children[A : Monoid](f: TreeMapper[T, A]) =  TreeMapper[T, A]{ tree =>
    traverse(tree, f)
  }

  def deep[A : Monoid](m: TreeMapper[T, A]): TreeMapper[T, A] =
    m | children(deep[A](m))

  def deepest[A : Monoid](m: TreeMapper[T, A]): TreeMapper[T, A] =
    children(deep[A](m)) | m

  def multi[A : Monoid](m: TreeMapper[T, A]): TreeMapper[T, A] =
    m + children(deep[A](m))

  def flatMap[U <: T, B](f: U => MatcherResult[U, B]) = TreeMapper[U, B] {tree =>
    f(tree)
  }
  def filter(f: T => Boolean) = TreeMapper[T, T]{ t =>
    if (f(t))  Some((t, t))
    else None
  }
  def filterPartial[U <: T](f: PartialFunction[U, Boolean]) = TreeMapper[T, U]{
    case t: U if f.isDefinedAt(t) && f(t) => Some((t, t))
    case _ => None
  }

  def collect[U <: T, A](f: PartialFunction[U, A]) = filterPartial[U]{case t => f.isDefinedAt(t)} map (x => List(f(x)))

  def update[U <: T](f: PartialFunction[U, U]) = TreeMapper[U, Unit] { tree =>
    if (f.isDefinedAt(tree)) Some((f(tree), Monoids.Void.zero))
    else None
  }

}