package tql


trait Combinators[T] { self: Traverser[T] =>

  import scala.reflect.ClassTag

  /**
   * Traverse the children of the tree
   * */
  def children[A : Monoid](f: TreeMapper[A]) =  TreeMapper[A]{ tree =>
    traverse(tree, f)
  }

  /**
   * Traverse the tree in a TopDown manner, stop when a transformation/traversal has succeeded
   * */
  def deep[A : Monoid](m: TreeMapper[A]): TreeMapper[A] =
    m | children(deep[A](m))

  /**
   * Traverse the tree in a BottomUp manner, stop when a transformation/traversal has succeeded
   * */
  def deepest[A : Monoid](m: TreeMapper[A]): TreeMapper[A] =
    children(deep[A](m)) | m

  /**
   * Same as deep, but does not sop when a transformation/traversal has succeeded
   * */
  def multi[A : Monoid](m: TreeMapper[A]): TreeMapper[A] =
    m + children(multi[A](m))

  def flatMap[B](f: T => MatcherResult[B]) = TreeMapper[B] {tree =>
    f(tree)
  }


  /**
   * Succeed if the partial function f applied on the tree is defined and return true
   * E stands for explicit. Used to distinguish it from the macro impl.
   * */
  def filterE[U <: T : ClassTag](f: PartialFunction[U, Boolean]) = TreeMapper[U]{
    case t: U if f.isDefinedAt(t) && f(t) => Some((t, t))
    case _ => None
  }

  /**
   * Same as filter but puts the results into a list
   * */
  def collect[A](f: PartialFunction[T, A])(implicit x: ClassTag[T]) = filterE[T]{case t => f.isDefinedAt(t)} map (x => List(f(x)))

  trait AllowedTransformation[+I, O]

  /**
   *  Transform a I into a T where both I and O are subtypes of T and where a transformation from I to O is authorized
   * */
  def updateE[I <: T : ClassTag, O <: T](f: PartialFunction[I, O])(implicit x: AllowedTransformation[I, O]) = TreeMapper[Unit] {
    case t: I if (f.isDefinedAt(t)) => Some((f(t), Monoids.Void.zero))
    case _ => None
  }

  import Monoids._
  def stateful[A](init: A)(f: A => TreeMapper[Stateful[A]]) = TreeMapper[Stateful[A]] { tree =>
     f(init)(tree)
  }

}