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
    u.symbolOf[T].asRoot.allLeafs.foreach(_.sym.owner.info) //weird hack so that the types are set in each symbol and the buildImpl function doesn't fail
    val allLeafs = u.symbolOf[T].asRoot.allLeafs.map(x => q"${x.sym.companion}")
    buildImpl[T, A](f, allLeafs: _*)
  }

  def buildFromTopSymbolOptimize[T : c.WeakTypeTag, A : c.WeakTypeTag](f: c.Tree): c.Tree = {
    u.symbolOf[T].asRoot.allLeafs.foreach(_.sym.owner.info) /*weird hack so that the types are set in
                                                              each symbol and the buildImpl function doesn't fail*/

    def traverseBranch(branch: Branch): (c.Tree, List[c.Tree]) = {
      branch.leafs.foreach(_.sym.owner.info)
      val parameter = TermName(c.freshName)
      val funcName = TermName(c.freshName)
      val func = q"""val $funcName = ${buildImpl[T, A](f, branch.leafs.map(x => q"${x.sym.companion}"): _*)}"""
      val leafsCase = cq"$parameter => $funcName($parameter)"
      val branches = branch.branches.map(x => traverseBranch(x)).unzip
      val branchCases = branches._1
      val otherFuncs = branches._2.flatten

      val cas = cq"""
          ($parameter: ${branch.sym}) => $parameter match {
            case ..$branchCases
            case $leafsCase
          }
       """
      (cas, func :: otherFuncs)
    }

    val parameter = TermName(c.freshName)
    val root = u.symbolOf[T].asRoot
    val branches = root.branches.map(x => traverseBranch(x)).unzip
    val branchCases = branches._1
    val funcs = branches._2.flatten
    val leafCases = buildCases[T, A](f, root.leafs.map(x => q"${x.sym.companion}"), parameter)


    val code = q"""
        ..$funcs
        ($parameter: ${implicitly[c.WeakTypeTag[T]]}) => $parameter match {
          case ..$leafCases
          case ..$branchCases
          case v => Some((v, implicitly[Monoid[${implicitly[c.WeakTypeTag[A]]}]].zero))
        }
    """
    c.echo(c.enclosingPosition, show(code))
    code
  }
}