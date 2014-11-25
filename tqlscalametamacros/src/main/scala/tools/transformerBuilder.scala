package tools

/**
 * Created by Eric on 25.11.2014.
 */

import org.scalameta.adt._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context //TODO change to blackbox


abstract class TransformerBuilder[T] {

  protected def transformAdt: T = macro TransformerMacros.buildTransformerMatcher[T]

  protected def mapConserve[U <: AnyRef](elem: Iterable[U], f: U => U) = {
    var isModified = false
    val newSeq = elem.map(x => {
      val newX = f(x)
      isModified |= !(newX eq x)
      newX
    })
    if (!isModified) elem else newSeq
  }

  //TODO find a way to get rid of this redundant code
  protected def mapConserveOption[U <: AnyRef](elem: Option[U], f: U => U) = {
    var isModified = false
    val newSeq = elem.map(x => {
      val newX = f(x)
      isModified |= !(newX eq x)
      newX
    })
    if (!isModified) elem else newSeq
  }

  protected def flatMapConserve[U <: AnyRef](elem: Iterable[Iterable[U]], f: U => U) =
    mapConserve[Iterable[U]](elem, mapConserve[U](_, f))
}

class TransformerMacros(override val c: Context) extends TraverserMacros(c) {

  import c.universe._

  def buildTransformerMatcher[T: c.WeakTypeTag]: c.Tree = {
    u.symbolOf[T].asRoot.allLeafs.foreach(_.sym.owner.info)
    val (methodName, paramName) = ensureOwnerIsDelegateWithOneParam[T]
    u.symbolOf[T].asRoot.allLeafs.flatMap(buildCase2[T](_, methodName))
    u.symbolOf[T].asRoot.allLeafs.foreach(_.sym.owner.info)
    val cases = u.symbolOf[T].asRoot.allLeafs.flatMap(buildCase2[T](_, methodName))//buildCases[T](methodName)
    val default = TermName(c.freshName)
    val res = q"""
    $paramName match {
      case ..$cases
      case $default => $default
     }
    """
    res
  }

  case class Transformed(oldName: TermName, newName: TermName, stat: Option[c.Tree])

  def makeTransformStat[T : c.WeakTypeTag](field: Field, methodName: TermName): Transformed = {
    val newName = TermName(c.freshName(field.name.toString))
    field.tpe match {
      case t if t <:< weakTypeOf[T]           =>
        Transformed(field.name, newName, Some(q"val $newName = $methodName(${field.name}).asInstanceOf[$t]"))
      case t if t <:< weakTypeOf[Seq[T]]      =>
        Transformed(field.name, newName, Some(q"val $newName = mapConserve(${field.name}, $methodName).asInstanceOf[$t]"))
      case t if t <:< weakTypeOf[Seq[Seq[T]]] =>
        Transformed(field.name, newName, Some(q"val $newName = flatMapConserve(${field.name}, $methodName).asInstanceOf[$t]"))
      case t if t <:< weakTypeOf[Option[T]]   =>
        Transformed(field.name, newName, Some(q"val $newName = mapConserveOption(${field.name}, $methodName).asInstanceOf[$t]"))  //TODO change to a mapConserve like stuff
      case _ => Transformed(field.name, field.name, None)
    }
  }

  /** Get leaf = Term.Match(scrut: Term, cases: Seq[Case]) and methodName = transform
    * generate:
    * case origin @ Term.Match(scrut, cases) =>
    *   val a1 = transform(scrut).asInstanceOf[Term]
    *   val a2 = mapConserve(cases, transform).asInstanceOf[Seq[Case]]
    *   if ((a1 eq scrut) && (a2 eq cases)) origin else Term.Match(a1, a2)
   * */
  def buildCase2[T : c.WeakTypeTag](leaf: Leaf, methodName: TermName): Option[c.Tree] = {
    val listOfTransforms = leaf.nontriviaFields.map(makeTransformStat[T](_, methodName))
    val listOfTransfromWithStats = listOfTransforms.filter(!_.stat.isEmpty)
    if (listOfTransfromWithStats.size > 0) {
      val listOfParamNames = leaf.nontriviaFields.map(p => pq"${p.name} @ _")
      val origin = TermName(c.freshName("origin"))
      val pat = pq"$origin @ ${leaf.sym.companion}(..$listOfParamNames)"
      val isEqual = listOfTransfromWithStats.foldLeft[c.Tree](q"true")((acc, c) => q"$acc && (${c.newName} eq ${c.oldName})")
      val reconstructArgList = listOfTransforms.map(_.newName)
      val reconstruct = q"${leaf.sym.companion}(..$reconstructArgList)"
      val finalStmt = q"if ($isEqual) $origin else $reconstruct"
      val allStmts = listOfTransfromWithStats.flatMap(_.stat) ++ List(finalStmt)
      val cas = cq"$pat => ..$allStmts"
      Some(cas)
    }
    else
      None
  }
}