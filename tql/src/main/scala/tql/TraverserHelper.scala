package tql

/**
 * Created by Eric on 27.10.2014.
 */

import scala.collection.immutable.Seq
import scala.collection.mutable.ListBuffer
import scala.reflect._


object TraverserHelper {

  import MonoidEnhencer._

  /*def traverseSeq2[U, T <: U with AnyRef : ClassTag, A : Monoid](
                   f: Traverser[U]#Matcher[A],
                   seq: Seq[T]): Option[(Seq[T], A)] = {
    val (x, y) = seq.foldLeft((ListBuffer[T](), implicitly[Monoid[A]].zero)) { (acc, c) =>
      val (a : T, b) = f(c).get
      acc._1 += a
      (acc._1, b + acc._2)
    }
    Some((collection.immutable.Seq(x: _*), y))
  }

  def traverseSeqofSeq2[U, T <: U with AnyRef : ClassTag, A : Monoid](
                  f: Traverser[U]#Matcher[A],
                  seq: Seq[Seq[T]]): Option[(Seq[Seq[T]], A)] = {
    val (x, y) = seq.foldLeft((ListBuffer[Seq[T]](), implicitly[Monoid[A]].zero)) { (acc, c) =>
      val (a : Seq[T], b) = traverseSeq2(f, c).get
      acc._1 += a
      (acc._1, b + acc._2)
    }
    Some((collection.immutable.Seq(x: _*), y))
  }*/

  def traverseSeq2[U, T <: U with AnyRef : ClassTag, A : Monoid](
                  f: Traverser[U]#Matcher[A],
                  seq: Seq[T]): Option[(Seq[T], A)] = {
    val buffer = new ListBuffer[T]()
    buffer.sizeHint(seq.size)
    var hasChanged = false
    var i = 0
    val seqSize = seq.size
    var acc = implicitly[Monoid[A]].zero
    while (i < seqSize) {
      val old = seq(i)
      val (t: T, a) = f(old).get
      buffer.append(t)
      acc += a
      hasChanged |= !(old eq t)
      i += 1
    }
    Some((if (hasChanged) collection.immutable.Seq(buffer: _*) else seq, acc))
  }

  def traverseSeqofSeq2[U, T <: U with AnyRef : ClassTag, A : Monoid](
                        f: Traverser[U]#Matcher[A],
                        seq: Seq[Seq[T]]): Option[(Seq[Seq[T]], A)] = {
    val buffer = new ListBuffer[Seq[T]]()
    buffer.sizeHint(seq.size)
    var hasChanged = false
    var i = 0
    val seqSize = seq.size
    var acc = implicitly[Monoid[A]].zero
    while (i < seqSize) {
      val old = seq(i)
      val (t: Seq[T], a) = traverseSeq(f, old).get
      buffer  += t
      acc += a
      hasChanged |= !(old eq t)
      i += 1
    }
    Some((if (hasChanged) collection.immutable.Seq(buffer: _*) else seq, acc))
  }

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

  def optional2[U, T <: U with AnyRef : ClassTag, A: Monoid](
               f:  Traverser[U]#Matcher[A],
               a: Option[T]): Option[(Option[T], A)] = a match {
    case Some(x) => f(x) match {
      case Some((x: T, y)) => Some((Some(x), y))
      case _ => Some((None, implicitly[Monoid[A]].zero))
    }
    case _ => Some((None, implicitly[Monoid[A]].zero))
  }

  def optional[U, T <: U with AnyRef : ClassTag, A: Monoid](
              f:  Traverser[U]#Matcher[A],
              a: Option[T]): Option[(Option[T], A)] = Some(a
    .flatMap(f(_))
    .collect{case (x: T, y) if classTag[T].runtimeClass.isInstance(x) => (Some(x), y)}
    .getOrElse((None, implicitly[Monoid[A]].zero)))
}
