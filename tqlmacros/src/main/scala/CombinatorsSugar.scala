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
    q"guard[$lhs]($f)"
  }

  def updateSugarImpl[T : c.WeakTypeTag](c: Context)(f: c.Tree): c.Tree = {
    import c.universe._
    val (lhs, rhs) =  getGLBsfromPFs[T](c)(f)
    f match {
      case a @ q"{case ..$cases}" =>
        q"transform[$lhs, $rhs]{case ..$cases}"
    }
  }

  def getGLBsfromPFs[T : c.WeakTypeTag](c: Context)(f: c.Tree): (c.Type, c.Type) = {
    import c.universe._
    f match {
      case q"{case ..$cases}" =>
        val tpes: List[(c.Type,c.Type)] = cases.map(_ match {
          case cq"${lhs: c.Tree} => ${rhs:  c.Tree}" => (lhs.tpe, rhs.tpe)
          case _ => c.abort(c.enclosingPosition, "Bad format in partial function")
        })
        val (lhs, rhs) = tpes.unzip
        (lub(lhs), lub(rhs))
      case _ => c.abort(c.enclosingPosition, "Expecting a partial function here")
    }
  }
}
