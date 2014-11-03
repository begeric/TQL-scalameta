package tqlscalameta

/**
 * Created by Eric on 20.10.2014.
 */

import tql._
import scala.meta._

object ScalaMetaTraverser  extends Traverser[Tree] with Combinators[Tree] with SyntaxEnhancer[Tree] {

  import MonoidEnhencer._

  def traverse[A : Monoid](tree: Tree, f: TreeMapper[A]): MatcherResult[A] =
    ScalametaTraverserHelperMacros.buildFromTopSymbol[Tree, A](f)(tree)

}
