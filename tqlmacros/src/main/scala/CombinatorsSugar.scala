package tql
/**
 * Created by Eric on 26.10.2014.
 */

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

class CombinatorsSugar(val c: Context) {
  import c.universe._
  import c.universe.internal._

  /*
  * Taken from https://github.com/retronym/scala/blob/c17b92b7dd7398fd2524067a6cda96bb4d607aeb/src/compiler/scala/tools/nsc/ast/Trees.scala#L327
  * ABSOLUTELY DOESNT WORK
  * */
  object RemoveUnapply extends c.universe.Transformer {
    override def transform(tree: c.Tree): c.Tree = tree match {
      case UnApply(Apply(Select(qual, TermName("unapply")), List(Ident(TermName("<unapply-selector>")))), args) =>
        Apply(transform(qual), transformTrees(args))
      case _ => super.transform(tree)
    }

    def apply(tree: c.Tree): c.Tree =  c.untypecheck(transform(tree))
  }


  def filterSugarImpl[T : c.WeakTypeTag](f: c.Tree): c.Tree = {
    val (lhs, _) =  getLUBsfromPFs[T](f)
    q"${c.prefix}.guard[$lhs]($f)"
  }

  /*def updateWithResult[T : c.WeakTypeTag](c: Context)(f: c.Tree): c.Tree = {
    import c.universe._
    f match {
      case q"{case ..$cases}" =>
        val tpes: List[(c.Type, c.Type)] = cases.map(getTypesFromCase(c)(_))

      case _ => c.abort(c.enclosingPosition, "error in updateWithResult")
    }
  }  */


  def testImpl(f: c.Tree): c.Tree = {

    f match {
      case q"{case $cas => $_}" =>
        val cas2 = RemoveUnapply(cas)
        val tmp1 = showRaw(cas)
        val tmp2 = showRaw(cas2)
        c.abort(c.enclosingPosition, tmp1 + "\n" + tmp2)
    }
  }

  def TWithResult[T: c.WeakTypeTag](t: c.Tree): c.Tree = q"($t, tql.Monoid.Void.zero)"

  def rewriteSugarImpl[T : c.WeakTypeTag](f: c.Tree): c.Tree = {

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

  def updateSugarImpl2[T : c.WeakTypeTag](f: c.Tree): c.Tree = {
    import c.universe._
    f match {
      case q"{case ..$cases}" if cases.size == 2 =>

        val (lhs0, rhs0) =  getTypesFromCase(cases(0))
        val (lhs1, rhs1) =  getTypesFromCase(cases(1))
        //val CaseDef(pat, guard, body) = cases(0)
        val tmp1 = showRaw(RemoveUnapply(cases(0)))
        val tmp2 = showRaw(cq"scala.meta.interna.ast.Lit.Int(a) => scala.meta.interna.ast.Lit.Int(a)")
        c.abort(c.enclosingPosition, tmp1 + "\n\n" + tmp2)
        q"""
        transform[$lhs0, $rhs0](PartialFunction[$lhs0, $rhs0]((${RemoveUnapply(cases(0))}).asInstanceOf[PartialFunction[$lhs0, $rhs0]])) orElse
        transform[$lhs1, $rhs1](PartialFunction[$lhs1, $rhs1]((${RemoveUnapply(cases(1))}).asInstanceOf[PartialFunction[$lhs1, $rhs1]]))
        """
      case _ => c.abort(c.enclosingPosition, "error in update2Impl")
    }
  }

  def updateSugarImpl[T : c.WeakTypeTag](f: c.Tree): c.Tree = {
    import c.universe._
    val (lhs, rhs) =  getLUBsfromPFs[T](f)
    f match {
      case a /*@ q"{case ..$cases}"*/ =>
        //this is horrible, one should use c.untypecheck, but it doesn't work with extractors in pattern matching
        //see https://issues.scala-lang.org/browse/SI-8825
        //c.untypecheck(q"transform[$lhs, $rhs]{case ..$cases}")
        q"${c.prefix}.transform[$lhs, $rhs](PartialFunction[$lhs, $rhs](($a).asInstanceOf[PartialFunction[$lhs, $rhs]]))"
    }
  }


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
