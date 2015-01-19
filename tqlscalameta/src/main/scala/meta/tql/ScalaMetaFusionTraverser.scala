package scala.meta.tql

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

  implicit def materializerAllowedTransformation[I <: Tree, O <: Tree]: AllowedTransformation[I, O] =
    macro AllowedTransformationsMaterializer.materialize[scala.meta.internal.ast.Tree, I, O]

  def traverse[A : Monoid](tree: Tree, f: Matcher[A]): MatchResult[A] =
    ScalametaTraverserHelperMacros.buildFromTopSymbolDelegate[Tree, A](f,
      scala.meta.internal.ast.Term.Name,
      scala.meta.internal.ast.Lit.Char,
      scala.meta.internal.ast.Term.Apply,
      scala.meta.internal.ast.Lit.Int,
      scala.meta.internal.ast.Type.Name,
      scala.meta.internal.ast.Term.Param,
      scala.meta.internal.ast.Type.Apply,
      scala.meta.internal.ast.Term.ApplyInfix
    )

  def TtoU(t: Tree): Int = t.$tag
  def optimize[A](m: Matcher[A]): Matcher[A] = macro ScalametaFusionTraverserHelperMacros.getAllTags
}
