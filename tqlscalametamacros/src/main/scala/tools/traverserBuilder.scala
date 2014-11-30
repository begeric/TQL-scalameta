package tools

/**
 * Created by Eric on 25.11.2014.
 */

import org.scalameta.adt._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context //TODO change to blackbox

abstract class TraverserBuilder[T] {

	protected def traverseAdt: Unit = macro TraverserMacros.buildTraverserMatcher[T]
  protected def traverseAdt2: Unit = macro TraverserMacros.buildTraverserMatcher2[T]

}

class TraverserMacros(val c: Context) extends AdtReflection {
  val u: c.universe.type = c.universe
	import c.universe._

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

  def buildTraverserMatcher2[T : c.WeakTypeTag]: c.Tree = {
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