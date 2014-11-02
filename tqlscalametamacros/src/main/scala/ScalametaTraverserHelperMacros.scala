/**
 * Created by Eric on 29.10.2014.
 */
package tqlscalameta

import tql._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context


object ScalametaTraverserHelperMacors {
  def build[T, A](f: Any /*temporary*/, objs: Any*): (T => Option[(T, A)]) = macro ScalametaTraverserBuilder.buildImpl[T, A]

  def buildFromTopSymbol[T, A](f: Any): (T => Option[(T, A)]) = macro ScalametaTraverserBuilder.buildFromTopSymbol[T, A]
}

class ScalametaTraverserBuilder(override val c: Context) extends TraverserBuilder(c) with org.scalameta.adt.AdtReflection {
  val u: c.universe.type = c.universe
  import c.universe._

  def buildFromTopSymbol[T : c.WeakTypeTag, A : c.WeakTypeTag](f: c.Tree): c.Tree = {
    u.symbolOf[T].asRoot.allLeafs.foreach(_.sym.owner.info) //weird hack so that the types are set in each symbol and the buildImpl function doesn't fail
    val allLeafs = u.symbolOf[T].asRoot.allLeafs.map(x => q"${x.sym.companion}")
    buildImpl[T, A](f, allLeafs: _*)
  }
}