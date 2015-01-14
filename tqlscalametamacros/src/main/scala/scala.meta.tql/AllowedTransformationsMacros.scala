package scala.meta.tql

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
    val Tout = u.symbolOf[O].asType
    if (brlhs.exists(x => Tout.toType <:< x.info.typeSymbol.asType.toType))
      c.Expr(q"new tql.AllowedTransformation[${implicitly[c.WeakTypeTag[I]]}, ${implicitly[c.WeakTypeTag[O]]}] {}")
    else
      c.abort(c.enclosingPosition,
        "impossible to materialize AllowedTransformations[" +
          show(implicitly[c.WeakTypeTag[I]].tpe) + ", " +
          show(implicitly[c.WeakTypeTag[O]].tpe) + "]" + "\n" +
          "because " + show(Tout.sym.fullName) + " is not a subtype of any in " + show(brlhs) + "\n")
  }

  private def getBranch[A : c.WeakTypeTag]: List[TypeSymbol] = {
    val Asym = u.symbolOf[A]
    if (Asym.isBranch) List(Asym.asType)
    else if (Asym.isLeaf)
      Asym.asLeaf.sym.asClass.baseClasses
        .filter(_.isBranch)
        .filter(_.asBranch.leafs.exists(x => x.sym.fullName == Asym.asLeaf.sym.fullName))//gotta find a better solution
        .map(_.asType)
    else c.abort(c.enclosingPosition, show("impossible to get branch from  "  + show(implicitly[c.WeakTypeTag[A]])))
  }


}
