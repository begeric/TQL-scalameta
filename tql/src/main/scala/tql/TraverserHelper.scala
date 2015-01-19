package tql

/**
 * Created by Eric on 27.10.2014.
 */

import scala.collection.immutable.Seq
import scala.collection.mutable.ListBuffer
import scala.reflect._

object TraverserHelper {
  import MonoidEnhencer._


  def traverseSeq[U, T <: U with AnyRef : ClassTag, A : Monoid](
                 f: Traverser[U]#Matcher[A],
                 seq: Seq[T]): Option[(Seq[T], A)] = {
    val m = implicitly[Monoid[A]]
    var buffer = new ListBuffer[T]()
    var hasChanged = false
    var acc = m.zero
    for {t <- seq
         (a1 : T, a2) <- f(t)
         if classTag[T].runtimeClass.isInstance(a1)
    } {
        buffer.append(a1)
        acc += a2
        hasChanged |= !(a1 eq t)
    }
    hasChanged |= seq.size != buffer.size //TODO this should be an error
    Some((if (hasChanged) collection.immutable.Seq(buffer: _*) else seq, acc))
  }

  def traverseSeqofSeq[U, T <: U with AnyRef : ClassTag, A : Monoid](
                      f:  Traverser[U]#Matcher[A],
                      seq: Seq[Seq[T]]): Option[(Seq[Seq[T]], A)] = {
    val m = implicitly[Monoid[A]]
    var buffer = new ListBuffer[Seq[T]]()
    var hasChanged = false
    var acc = m.zero
    for {t <- seq
         (a1, a2) <- traverseSeq(f, t)
    }{
        buffer.append(a1)
        acc += a2
        hasChanged |= !(a1 eq t)
    }
    hasChanged |= seq.size != buffer.size //TODO this should be an error
    Some((if (hasChanged) collection.immutable.Seq(buffer: _*) else seq, acc))
  }

  def optional[U, T <: U with AnyRef : ClassTag, A: Monoid](
              f:  Traverser[U]#Matcher[A],
              a: Option[T]): Option[(Option[T], A)] = Some(a
    .flatMap(f(_))
    .collect{case (x: T, y) if classTag[T].runtimeClass.isInstance(x) => (Some(x), y)}
    .getOrElse((None, implicitly[Monoid[A]].zero)))
}
