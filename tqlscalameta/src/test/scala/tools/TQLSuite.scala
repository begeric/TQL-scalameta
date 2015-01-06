package tools

import org.scalatest.FunSuite
import tools.ScalaToTree.CompilerProxy
import scala.meta.tql.ScalaMetaTraverser._
import scala.meta.internal.ast._
import scala.meta.syntactic.show._

/**
 * Created by Eric on 04.01.2015.
 */
class TQLSuite extends FunSuite {

  val propaganda = ScalaToTree.load(System.getProperty("user.dir") + "/tqlscalameta/src/test/resources/Propaganda.scala")

  val getVals: Matcher[(List[String], Map[String, List[String]])] =
    aggregateUntil(
      collect{case Defn.Val(_, (b: Term.Name):: Nil,_, _) => b.name.toString},
      focus{case _: Defn.Def => true} feed { defn =>
        getVals.children.map(x => Map(defn.name.toString -> x._1) ++ x._2)
      }
    )
  val getValsInFunc = (focus{case _: Defn.Def => true} andThen getVals).downBreak.map(_._2)

  val getValsInFunc2: Matcher[Map[String, List[String]]] =
    (focus{case _: Defn.Def => true} feed { defn =>
      (until(
        collect{case Defn.Val(_, (b: Term.Name):: Nil,_, _) => b.name.toString},
        focus{case _: Defn.Def => true}
      ).map(x => Map(defn.name.toString -> x)) + getValsInFunc).children
    }).downBreak

  val getValsInFunc3 = fix[Map[String, List[String]]]{r =>
    @@[Defn.Def] feed { defn =>
      (r either collect{case Defn.Val(_, (b: Term.Name):: Nil,_, _) => b.name.toString})
      .downBreak
      .children
      .map{case (m, x) => Map(defn.name.toString -> x) ++ m}
    }
  }.downBreak

  //rule taken from the Obey project
  val listToSetBool = down(transform {
    case tt @ Term.Apply(t@Term.Select(Term.Apply(Term.Name("List"), _), Term.Name("toSet")), _) =>
      t andCollect tt.toString
  })

  test("listToSetBool") {
      val res = listToSetBool(propaganda)
      val newCode = res.tree.get
      assert(newCode != propaganda)
  }

  val propagandaFuncs = Map("main" -> List("u","v"), "test" -> List("x"), "test2" -> List("y"))

  test("getValsInFunc") {
    val res = getValsInFunc(propaganda).result
    assert(res == propagandaFuncs)
  }

  test("getValsInFunc2") {
    val res = getValsInFunc2(propaganda).result
    assert(res == propagandaFuncs)
  }


  test("getValsInFunc3") {
    val res = getValsInFunc3(propaganda).result
    assert(res == propagandaFuncs)
  }
}
