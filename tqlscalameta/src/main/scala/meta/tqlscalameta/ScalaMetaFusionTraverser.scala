package scala.meta.tqlscalameta

/**
 * Created by Eric on 09.12.2014.
 */

import scala.language.experimental.macros

import tql._
import scala.meta._

object ScalaMetaFusionTraverser extends Traverser[Tree]
                                  with Combinators[Tree]
                                  with SyntaxEnhancer[Tree]
                                  with CollectionLikeUI[Tree]
                                  with Fusion[Tree]
                                  with SetOptimized[Tree, Int] {
  import MonoidEnhencer._
  /**
   *  Somehow I have to put all those macros here instead of inside a BaseScalaMetaTraverser trait, otherwise some
   *  trees are never traversed o_O'..
   */

  implicit def materializerAllowedTransformation[T, I <: T, O <: T]: tql.AllowedTransformation[I, O] =
    macro AllowedTransformationsMaterializer.materialize[T, I, O]

  def traverse[A : Monoid](tree: Tree, f: Matcher[A]): MatcherResult[A] =
    ScalametaTraverserHelperMacros.buildFromTopSymbol[Tree, A](f)(tree)

  def TtoU(t: Tree): Int = t.$tag
  def optimize[A](m: Matcher[A]): Matcher[A] = macro ScalametaFusionTraverserHelperMacros.getAllTags
}
