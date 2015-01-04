package tools

import org.scalatest.FunSuite
import tools.ScalaToTree.CompilerProxy
import scala.meta.tql.ScalaMetaTraverser._
import scala.meta.internal.ast._
import scala.meta.syntactic.show._

/**
 * Created by Eric on 04.01.2015.
 */
class TestRule extends FunSuite {

  val compiler: CompilerProxy = ScalaToTree.loadCompiler
  val scalaTree = compiler.parseAndType(ScalaToTree.loadSource(System.getProperty("user.dir") + "/tqlscalameta/src/test/resources/Propaganda.scala"))

  val scalaMetaTree:scala.meta.Tree = compiler.scalaToMeta(scalaTree)

  //rule taken from the Obey project
  val listToSetBool = down(transform {
      case tt @ Term.Apply(t @ Term.Select(Term.Apply(Term.Name("List"), _), Term.Name("toSet")), _) =>
        t andCollect tt.toString
    })

  val res =  listToSetBool(scalaMetaTree)
  val newCode = res.tree.get
  //println(newCode.show[Code] + " \n\n" + res.result)

  assert(newCode != scalaMetaTree)
}
