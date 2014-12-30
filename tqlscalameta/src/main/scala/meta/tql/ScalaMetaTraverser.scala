package scala.meta.tql

/**
 * Created by Eric on 21.12.2014.
 */

import scala.language.experimental.macros
import tql._
import scala.meta._

object ScalaMetaTraverser extends Traverser[Tree]
                          with Combinators[Tree]
                          with SyntaxEnhancer[Tree]
                          with CollectionLikeUI[Tree]{
  import MonoidEnhencer._

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
   *  Branch, Leaf  in the Adt scala.meta sense of the term
   */
  implicit def materializerAllowedTransformation[T, I <: T, O <: T]: AllowedTransformation[I, O] =
    macro AllowedTransformationsMaterializer.materialize[T, I, O]

  def traverse[A : Monoid](tree: Tree, f: Matcher[A]): MatchResult[A] =
    ScalametaTraverserHelperMacros.buildFromTopSymbol[Tree, A](f)(tree)
}


object ScalaMetaTraverser2  extends Traverser[Tree]
                            with Combinators[Tree]
                            with SyntaxEnhancer[Tree]
                            with CollectionLikeUI[Tree]{
  import MonoidEnhencer._

  implicit def materializerAllowedTransformation[T, I <: T, O <: T]: AllowedTransformation[I, O] =
    macro AllowedTransformationsMaterializer.materialize[T, I, O]


  val traverseTable = ScalametaTraverserHelperMacros.buildTraverseTable[Tree]

  def traverse[A : Monoid](tree: Tree, f: Matcher[A]): MatchResult[A] =
    try {
      traverseTable(tree.$tag).apply[A](tree, f)
    }
    catch {
      case e => println(tree.getClass); throw e
    }
}

object ScalaMetaTraverser3  extends Traverser[Tree]
with Combinators[Tree]
with SyntaxEnhancer[Tree]
with CollectionLikeUI[Tree]{
  import MonoidEnhencer._

  implicit def materializerAllowedTransformation[T, I <: T, O <: T]: AllowedTransformation[I, O] =
  macro AllowedTransformationsMaterializer.materialize[T, I, O]


  val traverseTable = ScalametaTraverserHelperMacros.buildTraverseTable[Tree]


  def traverse[A : Monoid](tree: Tree, f: Matcher[A]): MatchResult[A] =
    ScalametaTraverserHelperMacros.buildFromTopSymbolDelegate[Tree, A](f)
}

/*object ScalaMetaTraverser3  extends Traverser[Tree]
                            with Combinators[Tree]
                            with SyntaxEnhancer[Tree]
                            with CollectionLikeUI[Tree]{
  import MonoidEnhencer._

  implicit def materializerAllowedTransformation[T, I <: T, O <: T]: AllowedTransformation[I, O] =
  macro AllowedTransformationsMaterializer.materialize[T, I, O]


  def traverse[A : Monoid](tree: Tree, f: Matcher[A]): MatchResult[A] =
    ScalametaTraverserHelperMacros.buildTraverseSwitch[Tree, A](f)(tree)
} */
