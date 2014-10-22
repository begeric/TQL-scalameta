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

  implicit object tree2tree extends AllowedTransformation[Tree, Tree]


  //def fp[T <: Tree, A](t: Tree): Option[(Tree, A)] = None

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


  def traverse[A : Monoid](tree: Tree, f: TreeMapper[A]): MatcherResult[A] = {
    val m = implicitly[Monoid[A]]

    def termMatcher = TraverserHelper.build[Tree,A](f,
      Term.This, Term.Name, Term.If, Term.Block, Term.ApplyInfix)

    def t = 1

    (tree match {
      case t: Term => termMatcher(t)
      case v => Some((v, m.zero))
      /*case t: Type => typeMatcher(t)
      case t: Pat => patMatcher(t)
      case t: Decl => declMatcher(t)
      case t: Defn => defnMatcher(t)
      case t: Ctor => ctorMatcher(t)
      case t: Param => paramMatcher(t)
      case t: TypeParam => typeParamMatcher(t)
      case t: Arg => argMatcher(t)
      case t: Enum => enumMatcher(t)
      case Param.Type.ByName(tpe) => traverse1(tpe, (a: Param.Type) => Param.Type.ByName(a))
      case Param.Type.Repeated(tpe) => traverse1(tpe, (a: Param.Type) => Param.Type.Repeated(a))
      case Pkg(ref, stats) => traverse2ts(ref, stats, (a: Term.Ref, b: Seq[Stmt.TopLevel]) => Pkg(a, b))
      case Import.Clause(ref, sels) => traverse2ts(ref, sels, (a: Term.Ref, b: Seq[Selector]) => Import.Clause(a, b))
      case Import.Rename(from, to) => traverse2(from, to, (a: Import.Name, b: Import.Name) => Import.Rename(a, b))
      case Import.Unimport(name) => traverse1(name, (a: Import.Name) => Import.Unimport(a))
      case t => auxMatcher(t)*/
    })
  }
}
