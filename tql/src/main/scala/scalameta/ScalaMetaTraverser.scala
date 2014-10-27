package scalameta

/**
 * Created by Eric on 20.10.2014.
 */

/**
 * Created by Eric on 19.10.2014.
 */

import tql._
import scala.collection.immutable.Seq
import scala.collection.mutable.ListBuffer
import scala.meta.Import.Selector
import scala.meta._

object ScalaMetaTraverser  extends Traverser[Tree] with Combinators[Tree] with SyntaxEnhancer[Tree] {

  import MonoidEnhencer._
  import scala.reflect.{ClassTag, classTag}

  implicit object term2Term extends AllowedTransformation[Term, Term]
  implicit object type2type extends AllowedTransformation[Type, Type]
  implicit object decl2decl extends AllowedTransformation[Decl, Decl]
  implicit object defn2defn extends AllowedTransformation[Defn, Defn]
  implicit object lit2lit extends AllowedTransformation[Lit, Lit]
  implicit object pat2pat extends AllowedTransformation[Pat, Pat]



  /*Those functions won't be here anymore soon*/
  def traverseSeq[T <: Tree: ClassTag, A : Monoid](f: TreeMapper[A], seq: Seq[T]): Option[(Seq[T], A)] = {
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

  def traverseSeqofSeq[T <: Tree: ClassTag, A : Monoid](f: TreeMapper[A], seq: Seq[Seq[T]]): Option[(Seq[Seq[T]], A)] = {
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

  def optional[T <: Tree: ClassTag, A: Monoid](f: TreeMapper[A], a: Option[T])/*: Option[Option[(T, A)]]*/ = Some(a
    .flatMap(f(_))
    .collect{case (x: T, y) if classTag[T].runtimeClass.isInstance(x) => (Some(x), y)}
    .getOrElse((None, implicitly[Monoid[A]].zero)))


  def traverse[A : Monoid](tree: Tree, f: TreeMapper[A]): MatcherResult[A] =
    TraverserHelperMacros.buildFromTopSymbol[Tree, A](f)(tree)
}
