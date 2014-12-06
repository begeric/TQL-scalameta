package tools

/**
 * Created by Eric on 25.11.2014.
 */

import org.scalameta.adt._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context //TODO change to blackbox

object TraverserBuilder {

	def traverseAdt[T]: Unit = macro TraverserMacros.buildTraverserMatcher[T]
  def traverseAdtBranchesFirst[T]: Unit = macro TraverserMacros.buildTraverserMatcherBranchesFirs[T]

  def buildTraverseTable[T]: Array[(T, T => Unit) => Unit] = macro TraverserMacros.buildTraverseTable[T]
  def buildTraverseTableWithMethod[T](traverseMethod: T => Unit): Array[T => Unit] = macro TraverserMacros.buildTraverseTableWithMethod[T]

}

class TraverserMacros(val c: Context) extends AdtReflection {
  val u: c.universe.type = c.universe
	import c.universe._

  def buildTraverseTable[T : c.WeakTypeTag]: c.Tree = {
    u.symbolOf[T].asRoot.allLeafs.foreach(_.sym.owner.info)

    val Ttpe = implicitly[c.WeakTypeTag[T]]
    val nbLeaves = u.symbolOf[T].asRoot.allLeafs.size * 2
    val array = TermName(c.freshName("table"))
    val tagTerm = TermName("$tag")
    val assigns: List[c.Tree] = u.symbolOf[T].asRoot.allLeafs.map(x =>
      q"""
        $array(${x.sym.companion}.$tagTerm) = ${buildFuncForCase[T](x)}
      """
    )
    val res = q"""
        val $array = new Array[($Ttpe, $Ttpe => Unit) => Unit]($nbLeaves)
        ..$assigns
        $array.toArray
    """
    res
  }

  def buildTraverseTableWithMethod[T : c.WeakTypeTag](traverseMethod: c.Tree): c.Tree = {
    u.symbolOf[T].asRoot.allLeafs.foreach(_.sym.owner.info)

    val Ttpe = implicitly[c.WeakTypeTag[T]]
    val nbLeaves = u.symbolOf[T].asRoot.allLeafs.size * 2
    val array = TermName(c.freshName("table"))
    val tagTerm = TermName("$tag")
    val assigns: List[c.Tree] = u.symbolOf[T].asRoot.allLeafs.map(x =>
      q"""
        $array(${x.sym.companion}.$tagTerm) = ${buildFuncForTraverse[T](x, traverseMethod)}
      """
    )
    val res = q"""
        val $array = new Array[$Ttpe => Unit]($nbLeaves)
        ..$assigns
        $array.toArray
    """
    res
  }

  def buildFuncForCase[T : c.WeakTypeTag](leaf: Leaf): c.Tree = {
    val Ttpe = implicitly[c.WeakTypeTag[T]]
    val treeParam = TermName(c.freshName("tree"))
    val traverseParam = TermName(c.freshName("traverse"))
    val cas = buildCase[T](leaf, traverseParam)
    val mat = cas.map(x => q"$treeParam match {case $x}").getOrElse(q"()")
    q"($treeParam: $Ttpe, $traverseParam: $Ttpe => Unit) => $mat"
  }

  def buildFuncForTraverse[T : c.WeakTypeTag](leaf: Leaf, traverseMethod: c.Tree): c.Tree = {
    val Ttpe = implicitly[c.WeakTypeTag[T]]
    val treeParam = TermName(c.freshName("tree"))
    val tr = buildTraverse[T](leaf, treeParam, traverseMethod)
    val mat = tr.getOrElse(q"()")
    q"($treeParam: $Ttpe) => $mat"
  }

  def buildTraverse[T : c.WeakTypeTag](leaf: Leaf, argName: TermName, traverseMethod: c.Tree): Option[c.Tree] = {
    val newVal = TermName("new" + c.freshName(argName.toString))
    val cast = q"val $newVal = $argName.asInstanceOf[${leaf.sym}]"
    val fieldsTraverse = leaf.nontriviaFields.flatMap(makeTraverseStat2[T](_, newVal, traverseMethod))
    if (fieldsTraverse.size > 0)
      Some(q"""$cast; ${fieldsTraverse.reduce[c.Tree]((a,b) => q"$a;$b")}""")
    else
      None
  }
  def makeTraverseStat2[T : c.WeakTypeTag](field: Field, objName: TermName, methodName: c.Tree): Option[c.Tree] = field.tpe match {
    case t if t <:< weakTypeOf[T]           => Some(q"$methodName($objName.${field.name})")
    case t if t <:< weakTypeOf[Seq[T]]      => Some(q"$objName.${field.name}.foreach($methodName(_))")
    case t if t <:< weakTypeOf[Seq[Seq[T]]] => Some(q"$objName.${field.name}.foreach(_.foreach($methodName(_)))")
    case t if t <:< weakTypeOf[Option[T]]   => Some(q"$objName.${field.name}.foreach($methodName(_))")
    case _ => None
  }

  /*Use the parameter of the enclosing method to create pattern match
  * through the whole hierarchy of T
  *
  * def traverser(tree: Tree): Unit = traverseAdt
  * generate:
  * def traverser(tree: Tree): Unit = tree match {
  *   case ..
  * }
  * */
	def buildTraverserMatcher[T : c.WeakTypeTag]: c.Tree = {
    u.symbolOf[T].asRoot.allLeafs.foreach(_.sym.owner.info)
    val (methodName, paramName) = ensureOwnerIsDelegateWithOneParam[T]
    val cases = buildCases[T](methodName)
		val res = q"""
    $paramName match {
      case ..$cases
      case _ => ()
     }
    """
    res
	}

  def buildTraverserMatcherBranchesFirs[T : c.WeakTypeTag]: c.Tree = {
    u.symbolOf[T].asRoot.allLeafs.foreach(_.sym.owner.info)
    val (methodName, paramName) = ensureOwnerIsDelegateWithOneParam[T]
    q"""
    $paramName match {
      case ..${u.symbolOf[T].asRoot.branches.map(x => buildCases[T](x.sym, methodName))}
     }
    """
  }

  def buildCases[T : c.WeakTypeTag](sym: Symbol, methodName: TermName): c.Tree = {
    val parameter = TermName(c.freshName)
    cq"""
      ($parameter: $sym) => $parameter match {
        case ..${sym.asBranch.allLeafs.flatMap(buildCase[T](_, methodName))}
        case _ => ()
      }
     """
  }

  def buildCases[T : c.WeakTypeTag](methodName: TermName): List[c.Tree] = {
    val sym = u.symbolOf[T]
    if (sym.isRoot) sym.asRoot.allLeafs.flatMap(buildCase[T](_, methodName))
    else if (sym.isBranch) sym.asBranch.allLeafs.flatMap(buildCase[T](_, methodName))
    else Nil
  }

  def makeTraverseStat[T : c.WeakTypeTag](field: Field, methodName: TermName): Option[c.Tree] = field.tpe match {
      case t if t <:< weakTypeOf[T]           => Some(q"$methodName(${field.name})")
      case t if t <:< weakTypeOf[Seq[T]]      => Some(q"${field.name}.foreach($methodName(_))")
      case t if t <:< weakTypeOf[Seq[Seq[T]]] => Some(q"${field.name}.foreach(_.foreach($methodName(_)))")
      case t if t <:< weakTypeOf[Option[T]]   => Some(q"${field.name}.foreach($methodName(_))")
      case _ => None
    }

  def buildCase[T : c.WeakTypeTag](leaf: Leaf, methodName: TermName): Option[c.Tree] = {
   val listOfTraverseStats = leaf.nontriviaFields.flatMap(makeTraverseStat[T](_, methodName))
   if (listOfTraverseStats.size > 0) {
     val listOfParamNames = leaf.nontriviaFields.map(p => pq"${p.name} @ _")
     Some(cq"${leaf.sym.companion}(..$listOfParamNames) => ..$listOfTraverseStats")
    }
    else
      None
  }

  def ensureOwnerIsDelegateWithOneParam[T : c.WeakTypeTag]: (TermName, TermName) = {
    val rootTpe = implicitly[c.WeakTypeTag[T]].tpe
    val isMethod = c.internal.enclosingOwner.isMethod
    if (!isMethod)
      c.abort(c.enclosingPosition,
        """traverseAdt is just a delegate,
          |please use it inside a method
          |of the form method(name: T): Unit""".stripMargin)
    val methodName = c.internal.enclosingOwner.asMethod.name
    val params = c.internal.enclosingOwner.asMethod.paramLists
    if (params.size != 1)
      c.abort(c.enclosingPosition, "wrong number of parameters: found "  + params.size + ", required 1")
    val uniqueParam = params.head.head
    val paramName = uniqueParam.name
    val paramTpe = uniqueParam.info
    if (!(paramTpe <:< rootTpe))
      c.abort(c.enclosingPosition, paramName.toString + " must be of type: " + show(rootTpe) + ", found: " + show(paramTpe))
    (methodName, paramName.toTermName)
  }
}