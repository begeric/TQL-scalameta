package tql
/**
 * Created by Eric on 26.10.2014.
 */

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object CombinatorsSugar {

  def filterSugarImpl[T : c.WeakTypeTag](c: Context)(f: c.Tree): c.Tree = {
    import c.universe._
    val (lhs, _) =  getGLBsfromPFs[T](c)(f)
    q"${c.prefix}.guard[$lhs]($f)"
  }

  def update2Impl[T : c.WeakTypeTag](c: Context)(f: c.Tree): c.Tree = {
    import c.universe._
    f match {
      case q"{case ..$cases}" if cases.size == 2 =>

        val (lhs0, rhs0) =  getTypesFromCase(c)(cases(0))
        val (lhs1, rhs1) =  getTypesFromCase(c)(cases(1))
        val CaseDef(pat, guard, body) = cases(0)
        c.abort(c.enclosingPosition, showRaw(pat) + " \n:\n " + showRaw(cq"x @ Lit.Int(a) => Lit.Int(a)"))
        q"""
        transform[$lhs0, $rhs0](PartialFunction[$lhs0, $rhs0]((${c.untypecheck(cases(0))}).asInstanceOf[PartialFunction[$lhs0, $rhs0]])) orElse
        transform[$lhs1, $rhs1](PartialFunction[$lhs1, $rhs1]((${c.untypecheck(cases(1))}).asInstanceOf[PartialFunction[$lhs1, $rhs1]]))
        """
      case _ => c.abort(c.enclosingPosition, "error in update2Impl")
    }
  }

  def updateSugarImpl[T : c.WeakTypeTag](c: Context)(f: c.Tree): c.Tree = {
    import c.universe._
    val (lhs, rhs) =  getGLBsfromPFs[T](c)(f)
    f match {
      case a @ q"{case ..$cases}" =>
        //this is horrible, one should use c.untypecheck, but it doesn't work with extractors in pattern matching
        //see https://issues.scala-lang.org/browse/SI-8825
        //c.untypecheck(q"transform[$lhs, $rhs]{case ..$cases}")
        q"${c.prefix}.transform[$lhs, $rhs](PartialFunction[$lhs, $rhs](($a).asInstanceOf[PartialFunction[$lhs, $rhs]]))"
    }
  }

  def getGLBsfromPFs[T : c.WeakTypeTag](c: Context)(f: c.Tree): (c.Type, c.Type) = {
    import c.universe._
    f match {
      case q"{case ..$cases}" =>
        val tpes: List[(c.Type,c.Type)] = cases.map(getTypesFromCase(c)(_))
        val (lhs, rhs) = tpes.unzip
        (lub(lhs), lub(rhs))
      case func if func.tpe <:< weakTypeOf[PartialFunction[_, _]] =>
        val lhs :: rhs :: Nil = func.tpe.typeArgs
        (implicitly[c.WeakTypeTag[T]].tpe, rhs)
      case _ => c.abort(c.enclosingPosition, "Expecting a partial function here")
    }
  }

  def getTypesFromCase(c: Context)(cas: c.Tree): (c.Type, c.Type) = {
    import c.universe._
    cas match {
      case cq"${lhs: c.Tree} => ${rhs:  c.Tree}" => (lhs.tpe, rhs.tpe)
      case cq"${lhs: c.Tree} if $_ => ${rhs:  c.Tree}" => (lhs.tpe, rhs.tpe)
      case p => c.abort(c.enclosingPosition, "Bad format in partial function at: " + show(p))
    }
  }
}
