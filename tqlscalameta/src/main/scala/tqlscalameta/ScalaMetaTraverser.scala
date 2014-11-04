package tqlscalameta

/**
 * Created by Eric on 20.10.2014.
 */

import tql._
import scala.meta._

object ScalaMetaTraverser extends Traverser[Tree] with Combinators[Tree] with SyntaxEnhancer[Tree] {

  import MonoidEnhencer._
  import scala.language.experimental.macros

  /**
   * Create an AllowedTransformation[I, O] If :
   *  Upper Branch* of O is a subtype of Upper Branch of I
   *  Where I and O are both subtypes of T.
   *  Example:
   *  materializerAllowedTransformation[Tree, Lit.Int, Lit] would work since:
   *  Lit.Int is a Leaf* => its upper branch is Lit and Lit is a subtype of Lit
   *
   *  But materializerAllowedTransformation[Tree, Lit.Int, Term.If] wouldn't work
   *
   *  * Branch, Leaf  in the Adt scala.meta sense of the term
   *
   */
  implicit def materializerAllowedTransformation[T, I <: T, O <: T]: tql.AllowedTransformation[I, O] =
    macro AllowedTransformationsMaterializer.materialize[T, I, O]

  def traverse[A : Monoid](tree: Tree, f: TreeMapper[A]): MatcherResult[A] =
    ScalametaTraverserHelperMacros.buildFromTopSymbol[Tree, A](f)(tree)

}
