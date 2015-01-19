package meta.tql

import org.scalatest.FunSuite
import tools.{Traverser, ScalaToTree}
import tools.ScalaToTree.CompilerProxy
import tql.Monoid
import scala.meta.tql.ScalaMetaTraverser._
import scala.meta.internal.ast._
import tql.MonoidEnhencer._
import scala.meta.ui._
import scala.meta.syntactic._
import scala.meta.dialects.Scala211


/**
 * Created by Eric on 04.01.2015.
 */
class ObeyRuleSuite extends FunSuite {

  val compiler: CompilerProxy = ScalaToTree.loadCompiler

  val propaganda = ScalaToTree.load(System.getProperty("user.dir") + "/tqlscalameta/src/test/resources/Propaganda.scala")


  //rule taken from the Obey project
  val listToSetBool = topDown(transform {
    case tt @ Term.Apply(t@Term.Select(Term.Apply(Term.Name("List"), _), Term.Name("toSet")), _) =>
      t andCollect tt.toString
  })


  test("listToSetBool") {
      val res = listToSetBool(propaganda)
      val newCode = res.tree.get
      assert(newCode != propaganda)
  }


}
