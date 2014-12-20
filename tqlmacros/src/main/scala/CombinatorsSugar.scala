package tql
/**
 * Created by Eric on 26.10.2014.
 */

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

class CombinatorsSugar(val c: Context) {
  import c.universe._

  def macroCollect[T : c.WeakTypeTag]: c.Tree = {
    val tpe = implicitly[c.WeakTypeTag[T]]
    tpe.tpe match {
      case TypeRef(NoPrefix, TypeName("C"), List()) => q"${c.prefix}.CollectIn[${typeOf[List[_]]}]"
      case _ => q"${c.prefix}.CollectIn[$tpe]"
    }
    c.abort(c.enclosingPosition, showRaw(tpe.tpe))
  }


  def filterSugarImpl[T : c.WeakTypeTag](f: c.Tree): c.Tree = {
    val (lhs, _) =  getLUBsfromPFs[T](f)
    q"${c.prefix}.guard[$lhs]($f)"
  }

  def TWithUnitResult[T: c.WeakTypeTag](t: c.Tree): c.Tree = q"($t, tql.Monoid.Void.zero)"

  def TWithResult[T: c.WeakTypeTag, A : c.WeakTypeTag](a: c.Tree): c.Tree  = c.untypecheck(c.prefix.tree) match {
    case q"$_.CTWithResult[$_]($t)" => q"($t, $a)"
    case q"$_.CTWithResult[$_]($t).andCollect[$_]" => q"($t, $a)"
    case _ => c.abort(c.enclosingPosition, "Bad form in TWithResult " + show(c.prefix.tree))
  }

  def TAndCollect[T: c.WeakTypeTag, A : c.WeakTypeTag](a: c.Tree)(y: c.Tree): c.Tree = TWithResult[T, List[A]](q"($y.builder += $a).result")


  def transformSugarImpl[T : c.WeakTypeTag](f: c.Tree): c.Tree = {
    def getTypesFromTuple2(rhss: List[c.Type]): List[(c.Type, c.Type)] = rhss.map{
      /*It's not like I have a choice.. http://stackoverflow.com/questions/18735295/how-to-match-a-universetype-via-quasiquotes-or-deconstructors*/
      case TypeRef(_, sym, List(a, b)) if sym.fullName == "scala.Tuple2" => (a, b)
      case _ => c.abort(c.enclosingPosition, "There should be a Tuple2 here")
    }

    val (lhss, rhss) = getTypesFromPFS[T](f).unzip
    val lhs = lub(lhss)
    val (trhs, ress) = getTypesFromTuple2(rhss).unzip
    val rhs = lub(trhs)
    val res = lub(ress)

    q"${c.prefix}.transformWithResult[$lhs, $rhs, $res](PartialFunction[$lhs, ($rhs, $res)](($f).asInstanceOf[PartialFunction[$lhs, ($rhs, $res)]]))"
  }

  def transformSugarImplWithTRtype[T : c.WeakTypeTag](f: c.Tree)(r: c.Tree): c.Tree = transformSugarImpl[T](f)

  def getLUBsfromPFs[T : c.WeakTypeTag](f: c.Tree): (c.Type, c.Type) = {
    val tpes = getTypesFromPFS[T](f)
    if (tpes.size > 1){
      val (lhs, rhs) = tpes.unzip
      (lub(lhs), lub(rhs))
    }
    else
      tpes.head //TODO guarenteed to have size > 0?
  }

  def getTypesFromPFS[T : c.WeakTypeTag](f: c.Tree): List[(c.Type, c.Type)] = {
    f match {
      case q"{case ..$cases}" =>
        cases.map(getTypesFromCase(_))
      case func if func.tpe <:< weakTypeOf[PartialFunction[_, _]] =>
        val lhs :: rhs :: Nil = func.tpe.typeArgs
        List((implicitly[c.WeakTypeTag[T]].tpe, rhs))
      case _ => c.abort(c.enclosingPosition, "Expecting a partial function here")
    }
  }

  def getTypesFromCase(cas: c.Tree): (c.Type, c.Type) = {
    import c.universe._
    cas match {
      case cq"${lhs: c.Tree} => ${rhs:  c.Tree}" => (lhs.tpe, rhs.tpe)
      case cq"${lhs: c.Tree} if $_ => ${rhs:  c.Tree}" => (lhs.tpe, rhs.tpe)
      case p => c.abort(c.enclosingPosition, "Bad format in partial function at: " + show(p))
    }
  }
}
