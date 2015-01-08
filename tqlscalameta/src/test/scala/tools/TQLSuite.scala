package tools

import org.scalatest.FunSuite
import tools.ScalaToTree.CompilerProxy
import tql.Monoid
import scala.meta.tql.ScalaMetaTraverser._
import scala.meta.internal.ast._
import scala.meta.syntactic.show._
import tql.MonoidEnhencer._

/**
 * Created by Eric on 04.01.2015.
 */
class TQLSuite extends FunSuite {

  val propaganda = ScalaToTree.load(System.getProperty("user.dir") + "/tqlscalameta/src/test/resources/Propaganda.scala")

  val getVals: Matcher[(List[String], Map[String, List[String]])] = {
    tupledUntil(
      collect{case Defn.Val(_, (b: Term.Name):: Nil,_, _) => b.name.toString},
      focus{case _: Defn.Def => true} feed { defn =>
        getVals.children.map(x => Map(defn.name.toString -> x._1) ++ x._2)
      }
    )
  }
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

  /*def between[A, B: tql.Monoid, C : tql.Monoid](m1: Matcher[A], m2: Matcher[B], f: (A, B) => C): Matcher[C] = fix[C] {r =>
    (m1 feed { a =>
      def recur: Matcher[(C, B)] =
          r.map((_, implicitly[Monoid[B]].zero)) |
          (m2 feed {(cll: B) => recur.downBreak.children.map{case (c, b) => (c, cll + b)}})
      recur
      .downBreak
      .children
      .map{case (c, b) => f(a, b) + c}
    })
  }*/
  def between[A, B: tql.Monoid, C : tql.Monoid](m1: Matcher[A], m2: Matcher[B], f: (A, B) => C): Matcher[C] = {
    def inner: Matcher[(B, C)] = tupledUntil(
      m2,
      m1 feed { a =>
        inner.children.map{case (b, c) => f(a, b) + c}
      }
    )
    (m1 andThen inner).downBreak.map(_._2)
  }

  def repsep[A, B: tql.Monoid](m1: Matcher[A], m2: Matcher[B]): Matcher[Map[A, B]] =
    between[A, B, Map[A, B]](m1, m2, (a: A, v: B) => Map(a -> v))

  val getValsInFunc4 = repsep(
    visit{case f: Defn.Def => f.name.toString},
    collect{case Defn.Val(_, (b: Term.Name):: Nil,_, _) => b.name.toString}
  ).downBreak


  def group[K, V: tql.Monoid](key: Matcher[K], values: Matcher[V]) = {
    def inner: Matcher[(V, Map[K, V])] = tupledUntil(
      values,
      key feed { k =>
        inner.children.map{case (v, m) => Map(k -> v) ++ m}
      }
    )
    (key andThen inner).downBreak.map(_._2)
  }

  val getValsInFunc5 = group(
    visit{case f: Defn.Def => f.name.toString},
    collect{case Defn.Val(_, (b: Term.Name):: Nil,_, _) => b.name.toString}
  )

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

  val propagandaFuncs = Map("main" -> List("u","v"), "test" -> List("x"), "test2" -> List("y", "z"))

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

  test("getValsInFunc4") {
    val res = getValsInFunc4(propaganda).result
    assert(res == propagandaFuncs)
  }

  test("getValsInFunc5") {
    val res = getValsInFunc5(propaganda).result
    assert(res == propagandaFuncs)
  }
}
