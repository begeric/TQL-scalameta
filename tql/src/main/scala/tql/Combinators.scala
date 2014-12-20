package tql


import scala.collection.mutable
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

  trait Collector[C, A] {
    type R
    val builder: mutable.Builder[A, R]
    val monoid: Monoid[R]
  }
  object Collector {
    implicit def nothingToList[A](implicit y: CanBuildFrom[List[A], A, List[A]], m: Monoid[List[A]]) = new Collector[Nothing, A] {
      type R = List[A]
      val builder = y()
      val monoid = m
    }

    implicit def otherToCollect[A, C[A]](implicit y: CanBuildFrom[C[A], A, C[A]], m: Monoid[C[A]]) = new Collector[C[A], A] {
      type R = C[A]
      val builder = y()
      val monoid = m
    }
  }

  abstract class CollectInType[C[_]] {
    def apply[A](f: PartialFunction[T, A])(implicit  x: ClassTag[T], y: Collector[C[A], A]): Matcher[y.R]
  }

  /**
   * Same as filter but puts the results into a list
   * */
  def collect[C[_]] = new CollectInType[C] {
    def apply[A](f: PartialFunction[T, A])(implicit x: ClassTag[T], y: Collector[C[A], A]): Matcher[y.R] =
      Matcher[y.R] { tree =>
        if (f.isDefinedAt(tree)) Some((tree, (y.builder += f(tree)).result))
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


  /**
   * Syntactic sugar for transform combinator so that one doesn't need to type the type parameter
   * */
  def transform[A](f: PartialFunction[T, (T,A)]): Matcher[A] = macro CombinatorsSugar.transformSugarImpl[T]

  /**
   * Syntactic sugar for guard combinator so that one doesn't need to type the type parameter
   * */
  def filter(f: PartialFunction[T, Boolean]): Matcher[T] = macro CombinatorsSugar.filterSugarImpl[T]


}