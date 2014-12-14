package tql


import scala.reflect.ClassTag
import scala.collection.generic.CanBuildFrom
import scala.language.experimental.macros
import scala.language.higherKinds
import scala.language.implicitConversions
/**
 * Created by Eric on 20.10.2014.
 */

trait Combinators[T] { self: Traverser[T] =>

  /**
   * Traverse the children of the tree
   * */
  def children[A : Monoid](f: => Matcher[A]) = Matcher[A]{ tree =>
    traverse(tree, f)
  }

  /**
   * Traverse the tree in a TopDown manner, stop when a transformation/traversal has succeeded
   * */
  def downBreak[A : Monoid](m: Matcher[A]): Matcher[A] =
    m | children(downBreak[A](m))

  /**
   * Traverse the tree in a BottomUp manner, stop when a transformation/traversal has succeeded
   * */
  def upBreak[A : Monoid](m: Matcher[A]): Matcher[A] =
    children(upBreak[A](m)) | m

  /**
   * Same as TopDown, but does not sop when a transformation/traversal has succeeded
   * */
  def down[A : Monoid](m: Matcher[A]): Matcher[A] =
    m + children(down[A](m))

  /**
   * Same as upBreak, but does not sop when a transformation/traversal has succeeded
   * */
  def up[A : Monoid](m: => Matcher[A]): Matcher[A] =
    children(up[A](m)) + m

  /**
   * Succeed if the partial function f applied on the tree is defined and return true
   * */
  def guard[U <: T : ClassTag](f: PartialFunction[U, Boolean]) = Matcher[U]{
    case t: U if f.isDefinedAt(t) && f(t) => Some((t, t))
    case _ => None
  }

  /**
   *  Transform a I into a T where both I and O are subtypes of T and where a transformation from I to O is authorized
   * */
  def transform[I <: T : ClassTag, O <: T](f: PartialFunction[I, O])(implicit x: AllowedTransformation[I, O]) =
    Matcher[Unit] {
      case t: I if f.isDefinedAt(t) => Some((f(t), Monoid.Void.zero))
      case _ => None
    }

  /**
   *  Same as Transform, but can return a result too
   * */
  def transformWithResult[I <: T : ClassTag, O <: T, A](f: PartialFunction[I, (O, A)])(implicit x: AllowedTransformation[I, O]) =
    Matcher[A] {
      case t: I if f.isDefinedAt(t) => Some(f(t))
      case _ => None
    }


  /**
   *  Traverse the data structure and return a result
   * */
  def visit[A](f: PartialFunction[T, A])(implicit x: ClassTag[T]) =
    guard[T]{case t => f.isDefinedAt(t)} map(f(_))

  /**
   * WIP
   * */
  def stateful[A, B](init: => A)(f: ( => A) => Matcher[(B, A)]): Matcher[B] = {
    var state = init
    f(state) map {case (res, s) =>
      state = s
      res
    }
  }

  /**
   * Same as filter but puts the results into a list
   * */

  def collect[A : ClassTag](f: PartialFunction[T, A])(implicit x: ClassTag[T]) = Matcher[List[A]]{ tree =>
    if (f.isDefinedAt(tree)) Some((tree, List(f(tree))))
    else None
  }

  def collectIn[C[_]] = new {
    def apply[A](f: PartialFunction[T, A])(implicit  x: ClassTag[T], y: CanBuildFrom[C[A], A, C[A]]) =
      Matcher[C[A]]{ tree =>
        if (f.isDefinedAt(tree)) Some((tree, (y() += f(tree)).result))
        else None
      }
  }

  def collectIn2[V[_, _]] = new {
    def apply[A, B](f: PartialFunction[T, (A, B)])(implicit  x: ClassTag[T], y: CanBuildFrom[V[A, B], (A, B), V[A, B]]) =
      guard[T]{case t => f.isDefinedAt(t)} map(t => (y() += f(t)).result)
  }

  implicit def TWithResult[U <: T](t: U): (U, Unit) = macro CombinatorsSugar.TWithUnitResult[U]//(t, Monoid.Void.zero)
  implicit class CTWithResult[U <: T](t: U) {
    def withResult[A](a: A): (U, A) = macro CombinatorsSugar.TWithResult[U, A]//(t, a)
    def andCollect[A](a: A): (U, List[A]) = macro CombinatorsSugar.TAndCollect[U, A]//(t, List(a))
  }

  def rewrite(f: PartialFunction[T, (T,Any)]): Matcher[Any] = macro CombinatorsSugar.rewriteSugarImpl[T]

  def test[A](f: PartialFunction[A, Boolean]): Unit = macro CombinatorsSugar.testImpl

  /**
   * Syntactic sugar for guard combinator so that one doesn't need to type the type parameter
   * */
  def filter(f: PartialFunction[T, Boolean]): Matcher[T] = macro CombinatorsSugar.filterSugarImpl[T]

  /**
  * Syntactic sugar for transform combinator so that one doesn't need to type the type parameter
    * */
  def update(f: PartialFunction[T, T]): Matcher[Unit] = macro CombinatorsSugar.updateSugarImpl[T]


}