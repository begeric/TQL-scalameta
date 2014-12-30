package tql

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

/**
 * Created by Eric on 30.12.2014.
 */
object scalametaMacros {
  def precalculatedTags[T]: Map[String, Int] = macro ScalaMetaMacrosBundle.precalculatedTags[T]
}

class ScalaMetaMacrosBundle(val c: Context) extends org.scalameta.adt.AdtReflection {
  val u: c.universe.type = c.universe
  import c.universe._

  def getAllSubClass(root: ClassSymbol): List[Symbol] = {
    val sub = root.sym.asClass.knownDirectSubclasses.toList
    sub.flatMap(x => x :: getAllSubClass(x.asClass))
  }

  def getAllLeaves(root: Root) =
    getAllSubClass(root.sym.asClass).toSet.filter(x => x.isAdt && !x.isBranch).map(_.asLeaf).toList

  def precalculatedTags[T : c.WeakTypeTag]: c.Tree  = {
    val leaves = getAllLeaves(u.symbolOf[T].asRoot)
    val tagTerm = TermName("$tag")

    val elems = leaves.map(x => {
      //val s = Symbol(x.sym.fullName)
      //c.abort(c.enclosingPosition, show(x.sym.info.companion.members))
      q"(${x.sym.fullName}, ${x.sym.companion}.$tagTerm)"
    }) //${x.sym.companion}.$tagTerm
    val res =
      q"""
         import scala.meta.internal.ast._
         Map(..$elems)
      """
    //c.abort(c.enclosingPosition, show(res))
    res
    //c.abort(c.enclosingPosition, show(Primary.$tag))
  }

}

