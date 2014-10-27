package scalameta

/**
 * Created by Eric on 20.10.2014.
 */

import tql._
import scala.meta._

object ScalaMetaTraverser  extends Traverser[Tree] with Combinators[Tree] with SyntaxEnhancer[Tree] {

  import MonoidEnhencer._

  implicit object term2Term extends AllowedTransformation[Term, Term]
  implicit object type2type extends AllowedTransformation[Type, Type]
  implicit object decl2decl extends AllowedTransformation[Decl, Decl]
  implicit object defn2defn extends AllowedTransformation[Defn, Defn]
  implicit object lit2lit extends AllowedTransformation[Lit, Lit]
  implicit object pat2pat extends AllowedTransformation[Pat, Pat]

  def traverse[A : Monoid](tree: Tree, f: TreeMapper[A]): MatcherResult[A] =
    TraverserHelperMacros.buildFromTopSymbol[Tree, A](f)(tree)
}
