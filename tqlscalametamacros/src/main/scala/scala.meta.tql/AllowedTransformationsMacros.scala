package scala.meta.tqlscalameta

/**
 * Created by Eric on 03.11.2014.
 */

import tql._
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context


class AllowedTransformationsMaterializer(val c: Context) extends org.scalameta.adt.AdtReflection{
  val u: c.universe.type = c.universe
  import c.universe._

  def materialize[T : c.WeakTypeTag, I : c.WeakTypeTag, O : c.WeakTypeTag]: c.Expr[tql.AllowedTransformation[I, O]] = {
    val brlhs = getBranch[I]
    val brrhs = getBranch[O]
    if (brrhs.exists(x => brlhs.exists(y => x.info <:< y.info))) {
      c.Expr(q"new tql.AllowedTransformation[${implicitly[c.WeakTypeTag[I]]}, ${implicitly[c.WeakTypeTag[O]]}] {}")
    }
    else
      c.abort(c.enclosingPosition,
        "impossible to materialize AllowedTransformations[" +
          show(implicitly[c.WeakTypeTag[I]]) + ", " +
          show(implicitly[c.WeakTypeTag[O]]) + "]" + "\n" +
          "because " + show(brrhs) + " is not a subtype of " + show(brlhs) + "\n" +
          "info : \n" + show(brrhs.map(_.asClass.baseClasses.filter(_.isBranch))))
  }

  private def getBranch[A : c.WeakTypeTag]: List[TypeSymbol] = {
    if (u.symbolOf[A].isBranch) List(u.symbolOf[A].asType)
    else if (u.symbolOf[A].isLeaf) u.symbolOf[A].asLeaf.sym.asClass.baseClasses.filter(_.isBranch).map(_.asType)
    else c.abort(c.enclosingPosition, show("impossible to get branch from  "  + show(implicitly[c.WeakTypeTag[A]])))
  }


}
