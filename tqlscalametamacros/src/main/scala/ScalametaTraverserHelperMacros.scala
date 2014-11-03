package tqlscalameta

/**
 * Created by Eric on 29.10.2014.
 */

import tql._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context


object ScalametaTraverserHelperMacros {
  /**
   * Create a traverser on the types given in objs with the transformation function f
   * See tqlmacros.TraverserHelperMacros for more info
   * */
  def build[T, A](f: Traverser[T]#TreeMapper[A], objs: Any*): T => Option[(T, A)] =
    macro ScalametaTraverserBuilder.buildImpl[T, A]

  /**
   * Use the Adt reflexion from https://github.com/scalameta/scalameta/blob/master/foundation/adt/Reflection.scala
   * to generate the whole traverser from a root (here root = scala.meta.Tree)
   * */
  def buildFromTopSymbol[T, A](f: Traverser[T]#TreeMapper[A]): T => Option[(T, A)] =
    macro ScalametaTraverserBuilder.buildFromTopSymbol[T, A]
}

class ScalametaTraverserBuilder(override val c: Context)
                    extends TraverserBuilder(c)
                    with org.scalameta.adt.AdtReflection {
  val u: c.universe.type = c.universe
  import c.universe._

  def buildFromTopSymbol[T : c.WeakTypeTag, A : c.WeakTypeTag](f: c.Tree): c.Tree = {
    u.symbolOf[T].asRoot.allLeafs.foreach(_.sym.owner.info) /*weird hack so that the types are set in
                                                              each symbol and the buildImpl function doesn't fail*/

    val allBranches = u.symbolOf[T].asRoot.allBranches.map{x =>
        val parameter = TermName(c.freshName)
        val funcName = TermName(c.freshName)
        val func = q"""val $funcName = ${buildImpl[T, A](f, x.allLeafs.map(x => q"${x.sym.companion}"): _*)}"""
        val cas = cq"$parameter @ (_ : ${x.sym}) => $funcName($parameter)"
        (func, cas)
    }

    val parameter = TermName(c.freshName)
    val (funcs, cases) = allBranches.unzip
    q"""
        ..$funcs
        ($parameter: ${implicitly[c.WeakTypeTag[T]]}) => $parameter match {
          case ..$cases
          case v => Some((v, implicitly[Monoid[${implicitly[c.WeakTypeTag[A]]}]].zero))
        }
    """
  }
}