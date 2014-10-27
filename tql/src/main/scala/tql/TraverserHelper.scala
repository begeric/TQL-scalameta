package tql


import scala.collection.immutable.Seq
import scala.collection.mutable.ListBuffer
import scala.meta.Tree
import scala.reflect._


/**
 * Created by Eric on 27.10.2014.
 */
object TraverserHelper {

  import MonoidEnhencer._

  def traverseSeq[U, T  <: U with AnyRef : ClassTag, A : Monoid](f: Traverser[U]#TreeMapper[A], seq: Seq[T]): Option[(Seq[T], A)] = {
    val m = implicitly[Monoid[A]]
    var buffer = new ListBuffer[T]()
    var hasChanged = false
    var acc = m.zero
    for (t <- seq){
      f(t) match {
        case Some((a1: T, a2)) if classTag[T].runtimeClass.isInstance(a1) =>
          buffer.append(a1)
          acc += a2
          hasChanged |= !(a1 eq t)
        case _ =>
          hasChanged = true
      }
    }
    Some((if (hasChanged) collection.immutable.Seq(buffer: _*) else seq, acc))
  }

  def traverseSeqofSeq[U, T <: U with AnyRef: ClassTag, A : Monoid](f:  Traverser[U]#TreeMapper[A], seq: Seq[Seq[T]]): Option[(Seq[Seq[T]], A)] = {
    val m = implicitly[Monoid[A]]
    var buffer = new ListBuffer[Seq[T]]()
    var hasChanged = false
    var acc = m.zero
    for (t <- seq){
      traverseSeq(f, t) match {
        case Some((a1, a2))=>
          buffer.append(a1)
          acc += a2
          hasChanged |= !(a1 eq t)
        case _ =>
          hasChanged = true
      }
    }
    Some((if (hasChanged) collection.immutable.Seq(buffer: _*) else seq, acc))
  }

  def optional[U, T <: U with AnyRef: ClassTag, A: Monoid](f:  Traverser[U]#TreeMapper[A], a: Option[T])/*: Option[Option[(T, A)]]*/ = Some(a
    .flatMap(f(_))
    .collect{case (x: T, y) if classTag[T].runtimeClass.isInstance(x) => (Some(x), y)}
    .getOrElse((None, implicitly[Monoid[A]].zero)))
}
