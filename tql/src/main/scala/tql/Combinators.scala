package tql

/**
 * Created by Eric on 20.10.2014.
 */


import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.experimental.macros

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.collection.generic.CanBuildFrom

trait Combinators[T] { self: Traverser[T] =>

  def identity[A : Monoid] = Matcher[A]{tree => Some(tree, implicitly[Monoid[A]].zero)}

  def bfs[A : Monoid](m: => Matcher[A]): Matcher[A] = Matcher[A]{ tree =>
    val toVisit = new collection.mutable.Queue[T]()
    var result = implicitly[Monoid[A]].zero
    toVisit.enqueue(tree)

    val addToStack = Matcher[A]{ tree =>
      toVisit.enqueue(tree)
      /*
      In traverse we use the for construct to traverse trees, that's why we
      can't return None.
      This is an example of a (bad) leaking design decision.
      */
      Some(tree, implicitly[Monoid[A]].zero)
    }

    //vanilla bfs
    while (!toVisit.isEmpty){
      val top = toVisit.dequeue()
      m(top) match {
        case None => traverse(top, addToStack)
        case Some((t, r)) =>
          traverse(t, addToStack)
          result = implicitly[Monoid[A]].append(result, r)
      }
    }
    Some((tree, result))
  }

  /*def flatChildren[A : Monoid](f: => Matcher[A]) = Matcher[A] {tree =>
    lazy val toVisit = new collection.mutable.Queue[T]()
    var result = implicitly[Monoid[A]].zero

    val addToStack = Matcher[A]{ tree =>
      toVisit.enqueue(tree)
      /*
      In traverse we use the for construct to traverse trees, that's why we
      can't return None.
      This is an example of a (bad) leaking design decision.
      */
      Some(tree, implicitly[Monoid[A]].zero)
    }
    while (!toVisit.isEmpty) {
      val top = toVisit.dequeue()
    }

    Some((tree, result))
  } */

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

  trait Collector[C, A, R] {
    def builder: mutable.Builder[A, R]
  }
  object Collector {
    implicit def nothingToList[A](implicit y: CanBuildFrom[List[A], A, List[A]], m: Monoid[List[A]]) = new Collector[Nothing, A, List[A]] {
      def builder = {y().clear(); y()}
    }

    implicit def otherToCollect[A, C[A]](implicit y: CanBuildFrom[C[A], A, C[A]], m: Monoid[C[A]]) = new Collector[C[A], A, C[A]] {
      def builder = {y().clear(); y()}
    }
  }

  abstract class CollectInType[C[_]] {
    def apply[A, R](f: PartialFunction[T, A])(implicit  x: ClassTag[T], y: Collector[C[A], A, R]): Matcher[R]
  }

  /**
   * Same as focus but puts the results into a list
   * */
  def collect[C[_]] = new CollectInType[C] {
    def apply[A, R](f: PartialFunction[T, A])(implicit  x: ClassTag[T], y: Collector[C[A], A, R]): Matcher[R] =
      Matcher[R] { tree =>
        if (f.isDefinedAt(tree)) Some((tree, (y.builder += f(tree)).result))
        else None
      }
  }

  def collectIn2[V[_, _]] = new {
    def apply[A, B](f: PartialFunction[T, (A, B)])(implicit  x: ClassTag[T], y: CanBuildFrom[V[A, B], (A, B), V[A, B]]) =
      guard[T]{case t => f.isDefinedAt(t)} map(t => (y() += f(t)).result)
  }


  implicit class CTWithResult[U <: T](t: U) {
    def withResult[A](a: A): (U, A) = macro CombinatorsSugar.TWithResult[U, A]//(t, a)
    def andCollect[C[_]] = new {
        def apply[A, R](a: A)(implicit y: Collector[C[A], A, R]): (U, R) = macro CombinatorsSugar.TAndCollect[U, A]
      }
  }

  /**
   * Syntactic sugar for transform combinator so that one doesn't need to type the type parameter
   * */
  def transform[A](f: PartialFunction[T, Any]): Matcher[Any] = macro CombinatorsSugar.transformSugarImpl[T]

  /**
   * Syntactic sugar for guard combinator so that one doesn't need to type the type parameter
   * */
  def focus(f: PartialFunction[T, Boolean]): Matcher[T] = macro CombinatorsSugar.filterSugarImpl[T]

}