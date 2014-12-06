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
  def build[T, A](f: Traverser[T]#Matcher[A], objs: Any*): T => Option[(T, A)] =
    macro ScalametaTraverserBuilder.buildImpl[T, A]

  /**
   * Use the Adt reflexion from https://github.com/scalameta/scalameta/blob/master/foundation/adt/Reflection.scala
   * to generate the whole traverser from a root (here root = scala.meta.Tree)
   * */
  def buildFromTopSymbol[T, A](f: Traverser[T]#Matcher[A]): T => Option[(T, A)] =
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

  override def getParamsWithTypes(typ: c.Type): Option[(List[TermName], List[c.Type])] = {
    //c.abort(c.enclosingPosition, show(typ.companion.typeSymbol.asLeaf))
    val fields = typ.companion.typeSymbol.asLeaf.nontriviaFields
    if (!fields.isEmpty){
      Some(fields.map(x => (TermName(c.freshName), x.tpe) ).unzip)
    }
    else
      None
  }

  def buildFromTopSymbolOptimize[T : c.WeakTypeTag, A : c.WeakTypeTag](f: c.Tree): c.Tree = {
    u.symbolOf[T].asRoot.allLeafs.foreach(_.sym.owner.info) /*weird hack so that the types are set in
                                                              each symbol and the buildImpl function doesn't fail*/


    def buildFuncMaybe(f: c.Tree, objs: List[c.Tree]): Option[c.Tree] = {
      val parameter = TermName(c.freshName)
      val cases = buildCases[T, A](f, objs, parameter)
      if (cases.size > 0)
        Some(buildFuncWith[T, A](cases, parameter))
      else
        None
    }

    def traverseBranch(branch: Branch): (Option[c.Tree], List[c.Tree]) = {
      branch.leafs.foreach(_.sym.owner.info)
      val parameter = TermName(c.freshName)

      val leafsStuff = buildFuncMaybe(f, branch.leafs.map(x => q"${x.sym.companion}")).map{funcCode =>
        val funcName = TermName(c.freshName)
        val func = q"val $funcName = $funcCode"
        val leafsCase = cq"$parameter => $funcName($parameter)"
        (List(leafsCase), List(func))
      }.getOrElse((Nil, Nil))

      val branches = branch.branches.map(x => traverseBranch(x)).unzip
      val branchCases = branches._1.flatten
      val otherFuncs = branches._2.flatten

      val allcases = branchCases ++ leafsStuff._1

      if (allcases.size > 0){
        val cas = cq"""
          ($parameter: ${branch.sym}) => $parameter match {
            case ..$allcases
          }
       """
        (Some(cas), otherFuncs ++ leafsStuff._2)
      }
      else
        (None, otherFuncs)
    }

    val parameter = TermName(c.freshName)
    val root = u.symbolOf[T].asRoot
    val branches = root.branches.map(x => traverseBranch(x)).unzip
    val branchCases = branches._1.flatten
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
    code
  }
}