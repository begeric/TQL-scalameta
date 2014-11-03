package tqlscalameta

/**
 * Created by Eric on 03.11.2014.
 */

import tql._
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object AllowedTransformationsMacros {

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

}

class AllowedTransformationsMaterializer(val c: Context) extends org.scalameta.adt.AdtReflection{
  val u: c.universe.type = c.universe
  import c.universe._

  def materialize[T : c.WeakTypeTag, I : c.WeakTypeTag, O : c.WeakTypeTag]: c.Expr[tql.AllowedTransformation[I, O]] = {

    val brlhs = getBranch[I]
    val brrhs = getBranch[O]
    if (brrhs.info <:< brlhs.info)
      c.Expr(q"new tql.AllowedTransformation[${getBranch[I]}, ${getBranch[O]}] {}")
    else
      c.abort(c.enclosingPosition,
        "impossible to materialize AllowedTransformations[" +
          show(implicitly[c.WeakTypeTag[I]]) + ", " +
          show(implicitly[c.WeakTypeTag[O]]) + "]" + "\n" +
          "because " + show(brrhs) + " is not a subtype of " + show(brlhs))
  }

  private def getBranch[A : c.WeakTypeTag]: TypeSymbol = {
    if (u.symbolOf[A].isBranch) u.symbolOf[A].asBranch.sym.asClass.baseClasses.head.asType
    else if (u.symbolOf[A].isLeaf) u.symbolOf[A].asLeaf.sym.asClass.baseClasses.head.asType
    else c.abort(c.enclosingPosition, show("impossible to get branch from  "  + show(implicitly[c.WeakTypeTag[A]])))
  }


}
