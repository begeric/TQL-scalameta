package tqlscalameta


/**
 * Created by Eric on 09.12.2014.
 */

import tql._
import scala.meta._

object ScalaMetaFusionTraverser  extends Traverser[Tree] with Combinators[Tree] with SyntaxEnhancer[Tree] with CollectionLikeUI[Tree] {

  import MonoidEnhencer._
  import scala.language.experimental.macros

  implicit def materializerAllowedTransformation[T, I <: T, O <: T]: tql.AllowedTransformation[I, O] =
    macro AllowedTransformationsMaterializer.materialize[T, I, O]


  def traverse[A : Monoid](tree: Tree, f: Matcher[A]): MatcherResult[A] =
    ScalametaTraverserHelperMacros.buildFromTopSymbol[Tree, A](f)(tree)

  class FusedTopDown[+A : Monoid](val m1: Matcher[A]) extends Matcher[A] {
    override def compose[B >: A : Monoid](m2: => Matcher[B]): Matcher[B] = m2 match {
      case f: FusedTopDown[B] => new FusedTopDown[B](m1 + f.m1)
      case _=> super.compose(m2)
    }

    def apply(t: Tree) = (m1 + children(this)).apply(t)
  }


  override def down[A : Monoid](m: Matcher[A]): Matcher[A] = new FusedTopDown[A](m)

  class TagOptimized[+A](val elems: Set[Int], val m1: Matcher[A]) extends Matcher[A] {
    override def compose[B >: A : Monoid](m2: => Matcher[B]): Matcher[B] = m2 match {
      case f: TagOptimized[B] => new TagOptimized[B](elems + f.elems, m1 + f.m1)
      case _ => super.compose(m2)
    }

    def apply(tree: Tree) =
      if(elems contains tree.$tag)
        m1(tree)
      else
        None
  }

  def optimize[A](m: Matcher[A]): Matcher[A] = macro ScalametaFusionTraverserHelperMacros.getAllTags
}
