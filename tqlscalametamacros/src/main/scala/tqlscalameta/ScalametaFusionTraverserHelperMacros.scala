package tqlscalameta

/**
 * Created by Eric on 09.12.2014.
 */

import tql._
import scala.collection.mutable.ListBuffer
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

class ScalametaFusionTraverserHelperMacros(override val c: Context)
  extends TraverserBuilder(c)
  with org.scalameta.adt.AdtReflection {
  val u: c.universe.type = c.universe
  import c.universe._

  private def extractTypesFromPatten(pat: c.Tree): List[c.Type] = pat match {
    case pq"${first: c.Tree} | ..${rest: List[c.Tree]}" => first.tpe :: rest.map(_.tpe)
    case pq"${first: c.Tree}" => List(first.tpe)
    case _ => c.abort(c.enclosingPosition, "Cannot extract type from " + show(pat))
  }

  private def extractTypesFromCaseDef(cas: CaseDef): List[c.Type] = cas match{
    case cq"${lhs: c.Tree} => $_" => extractTypesFromPatten(lhs)
    case cq"${lhs: c.Tree} if $_ => $_" => extractTypesFromPatten(lhs)
    case _ => c.abort(c.enclosingPosition, "Cannot extract type from " + show(cas))
  }


  class ExtractTypes extends c.universe.Traverser {
    val tpes = new ListBuffer[c.Type]()

    override def traverse(tree: c.Tree): Unit = tree match {
      case q"{case ..${cases: List[CaseDef]}}" =>
        cases.foreach{cas: CaseDef =>
          tpes.appendAll(extractTypesFromCaseDef(cas))
        }
      case _ => super.traverse(tree)
    }

    def apply(tree: c.Tree): Set[c.Type] = {
      traverse(tree)
      tpes.toSet
    }
  }

  object ExtractTypes {
    def apply(tree: c.Tree): Set[c.Type] = (new ExtractTypes).apply(tree)
  }

  private def getTagsFromTypes(tpes: Set[c.Type]): Set[c.Tree] = {
    val tagTerm = TermName("$tag")
    tpes.flatMap{_ match {
        case tpe if tpe.typeSymbol.isLeaf => Set(q"${tpe.typeSymbol.companion}.$tagTerm")
        case tpe if tpe.typeSymbol.isBranch =>
          tpe.typeSymbol.asBranch.allLeafs.map { x =>
            q"${x.sym.companion}.$tagTerm"
          }.toSet
        case tpe if tpe.typeSymbol.isRoot =>  //maybe prevent this case????
          tpe.typeSymbol.asRoot.allLeafs.map { x =>
            q"${x.sym.companion}.$tagTerm"
          }.toSet
      }
    }
  }


  def getAllTags(m: c.Tree): c.Tree = {
    val tpes = ExtractTypes(m)
    val tags = getTagsFromTypes(tpes)
    q"new TagOptimized($tags, $m)"
  }
}
