package scala.meta.tql

/**
 * Created by Eric on 29.10.2014.
 */

import tql._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context


object ScalametaTraverserHelperMacros {

  def buildFromTopSymbolDelegate[T, A](f: Traverser[T]#Matcher[A], firsts: Any*): Traverser[T]#MatchResult[A] =
    macro ScalametaTraverserBuilder.buildFromTopSymbolDelegate[T, A]

}

class ScalametaTraverserBuilder(override val c: Context)
                    extends _root_.tql.TraverserBuilder(c)
                    with org.scalameta.adt.AdtReflection {
  val u: c.universe.type = c.universe
  import c.universe._

  def getAllLeaves(root: Root) = root.allLeafs

  def changeOrderOf(firsts: List[Symbol], allLeafs: List[Symbol]): List[Symbol] = {
    val tmp = firsts.map(_.fullName)
    val rest = for {leaf <- allLeafs if !(tmp contains leaf.fullName)} yield leaf
    val leafFist = firsts.map(x => allLeafs.find(_.fullName == x.fullName).get)
    leafFist ++ rest
  }

  def getAllLeafsOrderedInTree[T : c.WeakTypeTag](firsts: c.Tree*): List[c.Tree] = {
    val leaves: List[Leaf] = getAllLeaves(u.symbolOf[T].asRoot)
    //weird hack so that the types are set in each symbol and the buildImpl function doesn't fail
    leaves.foreach(_.sym.owner.info)
    val leafsWithOrder = changeOrderOf(firsts.map(_.symbol).toList, leaves.map(_.sym))
    leafsWithOrder.map(x => q"${x.companion}")
  }


  def buildFromTopSymbolDelegate[T : c.WeakTypeTag, A : c.WeakTypeTag](f: c.Tree, firsts: c.Tree*): c.Tree = {
    val allLeafs = getAllLeafsOrderedInTree[T](firsts: _*)
    buildImplDelegate[T, A](f, allLeafs: _*)
  }

  //trick to make it work with the Name unapply.
  override def getParamsWithTypes(typ: c.Type): Option[(List[TermName], List[c.Type])] = {
    val fields = typ.companion.typeSymbol.asLeaf.nontriviaFields
    if (!fields.isEmpty){
      Some(fields.map(x => (TermName(c.freshName), x.tpe) ).unzip)
    }
    else
      None
  }
}