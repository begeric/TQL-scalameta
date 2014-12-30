package scala.meta.tql

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


  abstract class TraversableFunc[T] {
    def apply[A: Monoid](tree: T, f: Traverser[T]#Matcher[A]): Traverser[T]#MatchResult[A]
  }

  def buildTraverseTable[T]: Array[TraversableFunc[T]] =
    macro ScalametaTraverserBuilder.buildTraverseTable[T]

}

class ScalametaTraverserBuilder(override val c: Context)
                    extends _root_.tql.TraverserBuilder(c)
                    with org.scalameta.adt.AdtReflection {
  val u: c.universe.type = c.universe
  import c.universe._



  def buildTraverseTable[T : c.WeakTypeTag]: c.Tree = {
    //weird hack so that the types are set in each symbol and the buildImpl function doesn't fail
    u.symbolOf[T].asRoot.allLeafs.foreach(_.sym.owner.info)
    val Ttpe = implicitly[c.WeakTypeTag[T]]
    val nbLeaves = u.symbolOf[T].asRoot.allLeafs.size * 2
    val array = TermName(c.freshName("table"))
    val tagTerm = TermName("$tag")

    val assigns: List[c.Tree] = u.symbolOf[T].asRoot.allLeafs.map(x =>
      q"""
        $array(${x.sym.companion}.$tagTerm) = ${buildFuncForTraverse[T](x)}
      """
    )
    val res = q"""
        val $array = new Array[scala.meta.tql.ScalametaTraverserHelperMacros.TraversableFunc[$Ttpe]]($nbLeaves)
        ..$assigns
        $array.toArray
    """
    res
  }

  private def buildFuncForTraverse[T : c.WeakTypeTag](leaf: Leaf): c.Tree = {
    val Ttpe = implicitly[c.WeakTypeTag[T]]
    val TtpeName = TypeName(c.freshName)
    val treeParam = TermName(c.freshName("tree"))
    val f = TermName(c.freshName)
    val cas = buildCase[T](f, leaf, treeParam)
    val mat = cas.map(x => q"$treeParam match {case $x}")
                 .getOrElse(q"Some(($treeParam, implicitly[Monoid[$TtpeName]].zero))")
    q"""
    new scala.meta.tql.ScalametaTraverserHelperMacros.TraversableFunc[$Ttpe] {
      def apply[$TtpeName: _root_.tql.Monoid]
      ($treeParam: $Ttpe, $f: _root_.tql.Traverser[$Ttpe]#Matcher[$TtpeName]) = $mat
      }
     """
  }

  private def makeTraverseStat[T : c.WeakTypeTag](f: TermName, field: Field): Option[c.Tree] = field.tpe match {
    case t if t <:< weakTypeOf[T] =>
      Some(q"$f(${field.name})")
    case t if t <:< weakTypeOf[scala.collection.immutable.Seq[T]] =>
      Some(q"_root_.tql.TraverserHelper.traverseSeq($f, ${field.name})")
    case t if t <:< weakTypeOf[scala.collection.immutable.Seq[scala.collection.immutable.Seq[T]]] =>
      Some(q"_root_.tql.TraverserHelper.traverseSeqofSeq($f, ${field.name})")
    case t if t <:< weakTypeOf[scala.Option[T]] =>
      Some(q"_root_.tql.TraverserHelper.optional($f, ${field.name})")
    case _ => None
  }

  private def buildTraversePart(leafSym: Symbol, fields: List[(Field, c.Tree)], orig: TermName): c.Tree = {
    val resultNamesForEachField       = fields.map(_ => (TermName(c.freshName), TermName(c.freshName)))
    val resultsNamesWithTraverseStmt  = resultNamesForEachField.zip(fields)
    val cqForEachField                = resultsNamesWithTraverseStmt.map(_ match {
      case ((newTree: TermName, newResult: TermName), (field: Field, stmt: c.Tree)) =>
        fq"($newTree: ${field.tpe}, $newResult @ _) <- $stmt"
    })

    val addMonoidResults = resultNamesForEachField.map(x => q"${x._2}").reduce[c.Tree]((a, b) => q"$a + $b")
    val newTreeNames = resultNamesForEachField.map(_._1)
    //it is here implied that Field.name will be the name of the variable containing the old tree
    //maybe need to change that and give the name as parameter.
    val newTreeWithOld = newTreeNames.zip(fields.map(_._1.name))

    //hope this is optimized away
    val eqList = newTreeWithOld.foldLeft[c.Tree](q"true")((a, b) => q"$a && (${b._1} eq ${b._2})")

    val reconstruct = q"${leafSym.companion}(..$newTreeNames)"

    val doesReconstruct = q"if($eqList) $orig else  $reconstruct"

    q"""
        for (
          ..$cqForEachField
        ) yield ($doesReconstruct, $addMonoidResults)
      """
  }

  private def buildCase[T : c.WeakTypeTag](f: TermName, leaf: Leaf, param: TermName): Option[c.Tree] = {
    val listOfTraverseStats = leaf.nontriviaFields.flatMap(makeTraverseStat[T](f, _))
    if (listOfTraverseStats.size > 0) {
      val listOfParamNames = leaf.nontriviaFields.map(p => pq"${p.name} @ _")
      val fieldsAndStats = leaf.nontriviaFields.zip(listOfTraverseStats)
      val traverse = buildTraversePart(leaf.sym, fieldsAndStats, param)
      Some(cq"${leaf.sym.companion}(..$listOfParamNames) => $traverse")
    }
    else
      None
  }



  /**
   * Naive case. Construct a big pattern match with all the leaves
   * */
  def buildFromTopSymbol[T : c.WeakTypeTag, A : c.WeakTypeTag](f: c.Tree): c.Tree = {
    //weird hack so that the types are set in each symbol and the buildImpl function doesn't fail
    u.symbolOf[T].asRoot.allLeafs.foreach(_.sym.owner.info)
    val allLeafs = u.symbolOf[T].asRoot.allLeafs.map(x => q"${x.sym.companion}")
    buildImpl[T, A](f, allLeafs: _*)
  }

  //trick to make it work with the Name unapply.
  override def getParamsWithTypes(typ: c.Type): Option[(List[TermName], List[c.Type])] = {
    //c.abort(c.enclosingPosition, show(typ.companion.typeSymbol.asLeaf))
    val fields = typ.companion.typeSymbol.asLeaf.nontriviaFields
    if (!fields.isEmpty){
      Some(fields.map(x => (TermName(c.freshName), x.tpe) ).unzip)
    }
    else
      None
  }

  /*It is useless, there are too many nested branches. So even to match the most used elements
  * the traverser needs to 'traverse' several branches*/
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