package tql
/**
 * Created by Eric on 26.10.2014.
 */

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object CombinatorsSugar {

  def filter(f: PartialFunction[Any, Boolean]): Any = macro CombinatorsSugar.filterSugarImpl
  def update(f: PartialFunction[Any, Any]): Any = macro CombinatorsSugar.updateSugarImpl


  def filterSugarImpl(c: Context)(f: c.Tree): c.Tree = {
    import c.universe._
    val (lhs, _) =  getGLBsfromPFs(c)(f)
    q"guard[$lhs]($f)"
  }

  def updateSugarImpl(c: Context)(f: c.Tree): c.Tree = {
    import c.universe._
    val (lhs, rhs) =  getGLBsfromPFs(c)(f)
    f match {
      case a @ q"{case ..$cases}" =>
        q"transform[$lhs, $rhs]{case ..$cases}"
    }
  }

  def getGLBsfromPFs(c: Context)(f: c.Tree): (c.Type, c.Type) = {
    import c.universe._
    f match {
      case q"{case ..$cases}" =>
        val tpes: List[(c.Type,c.Type)] = cases.map(_ match {
          case cq"($_ @ (_ : $tpe)) => $rhs" => (tpe.tpe, rhs.tpe)
          case cq"(_ : $tpe) => $rhs" => (tpe.tpe, rhs.tpe)
          case cq"${x: c.Tree}(..$_) => $rhs" => (x.tpe, rhs.tpe)
          case _ => c.abort(c.enclosingPosition, "Bad format in partial function")
        })
        val (lhs, rhs) = tpes.unzip
        (glb(lhs), glb(rhs))
      case _ => c.abort(c.enclosingPosition, "Expecting a partial function here")
    }
  }
}
