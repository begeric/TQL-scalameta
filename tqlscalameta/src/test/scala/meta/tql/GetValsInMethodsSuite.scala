package meta.tql

import org.scalatest.FunSuite
import tools.ScalaToTree.CompilerProxy
import tools.{ScalaToTree, Traverser}

import scala.meta.internal.ast.{Term, Defn}
import scala.meta.tql.ScalaMetaTraverser._
import tql.MonoidEnhencer._

/**
 * Created by Eric on 19.01.2015.
 */
class GetValsInMethodsSuite extends FunSuite {

  val compiler: CompilerProxy = ScalaToTree.loadCompiler

  val propaganda = ScalaToTree.load(System.getProperty("user.dir") + "/tqlscalameta/src/test/resources/Propaganda.scala")
  val propagandaFuncs = Map("main" -> List("u","v"), "test" -> List("x"), "test2" -> List("y", "z"))


  val getVals: Matcher[(List[String], Map[String, List[String]])] = {
    tupledUntil(
      collect{case Defn.Val(_, (b: Term.Name):: Nil,_, _) => b.name.toString},
      focus{case _: Defn.Procedure => true} feed { defn =>
        getVals.children.map(x => Map(defn.name.toString -> x._1) ++ x._2)
      }
    )
  }
  val getValsInMethods = (focus{case _: Defn.Procedure => true} andThen getVals).topDownBreak.map(_._2)


  def getValsInMethodsTraverser(tree: scala.meta.Tree) = {
    import scala.collection.mutable
    import scala.meta.internal.ast._
    val funcsWithVals = new mutable.HashMap[Term.Name, List[Term.Name]]()
    var currentFunc: Term.Name = null
    new Traverser {
      import scala.meta.internal.ast._
      override def traverse(tree: scala.meta.Tree): Unit = {
        tree match {
          case f: Defn.Procedure =>
            val oldFunc = currentFunc
            currentFunc = f.name
            super.traverse(tree)
            currentFunc = oldFunc
          case Defn.Val(_, (b: Term.Name):: Nil,_, rhs)
            if currentFunc != null =>
            val content = funcsWithVals.getOrElse(currentFunc,Nil)
            funcsWithVals += (currentFunc -> (b.name::content))
            super.traverse(tree)
          case _ => super.traverse(tree)
        }
      }
    }.traverse(tree)
    funcsWithVals.toMap
  }

  val getValsInMethods2: Matcher[Map[String, List[String]]] =
    (focus{case _: Defn.Procedure => true} feed { defn =>
      (until(
        collect{case Defn.Val(_, (b: Term.Name):: Nil,_, _) => b.name.toString},
        focus{case _: Defn.Procedure => true}
      ).map(x => Map(defn.name.toString -> x)) + getValsInMethods).children
    }).topDownBreak

  val getValsInMethods3 = fix[Map[String, List[String]]]{r =>
    @@[Defn.Procedure] feed { defn =>
      (r either collect{case Defn.Val(_, (b: Term.Name):: Nil,_, _) => b.name.toString})
        .topDownBreak
        .children
        .map{case (m, x) => Map(defn.name.toString -> x) ++ m}
    }
  }.topDownBreak

  def between[A, B: tql.Monoid, C : tql.Monoid](m1: Matcher[A], m2: Matcher[B], f: (A, B) => C): Matcher[C] = {
    def inner: Matcher[(B, C)] = tupledUntil(
      m2,
      m1 feed { a =>
        inner.children.map{case (b, c) => f(a, b) + c}
      }
    )
    (m1 andThen inner).topDownBreak.map(_._2)
  }

  def repsep[A, B: tql.Monoid](m1: Matcher[A], m2: Matcher[B]): Matcher[Map[A, B]] =
    between[A, B, Map[A, B]](m1, m2, (a: A, v: B) => Map(a -> v))

  val getValsInMethods4 = repsep(
    visit{case f: Defn.Procedure => f.name.toString},
    collect{case Defn.Val(_, (b: Term.Name):: Nil,_, _) => b.name.toString}
  ).topDownBreak


  def group[K, V: tql.Monoid](key: Matcher[K], values: Matcher[V]) = {
    def inner: Matcher[(V, Map[K, V])] = tupledUntil(
      values,
      key feed { k =>
        inner.children.map{case (v, m) => Map(k -> v) ++ m}
      }
    )
    (key andThen inner).topDownBreak.map(_._2)
  }

  val getValsInMethods5 = group(
    visit{case f: Defn.Procedure => f.name.toString},
    collect{case Defn.Val(_, (b: Term.Name):: Nil,_, _) => b.name.toString}
  )

  

  test("getValsInMethods scala.reflect") {
    val res = getValsInMethods(propaganda).result
    assert(res == propagandaFuncs)
  }

  test("getValsInMethods") {
    val res = getValsInMethods(propaganda).result
    assert(res == propagandaFuncs)
  }

  test("getValsInMethods2") {
    val res = getValsInMethods2(propaganda).result
    assert(res == propagandaFuncs)
  }

  /*test("getValsInMethods3") {
    val res = getValsInMethods3(propaganda).result
    assert(res == propagandaFuncs)
  } this is normal that it fails*/

  test("getValsInMethods4") {
    val res = getValsInMethods4(propaganda).result
    assert(res == propagandaFuncs)
  }

  test("getValsInMethods5") {
    val res = getValsInMethods5(propaganda).result
    assert(res == propagandaFuncs)
  }

  test("getValsInMethod") {
    val res = getValsInMethodsTraverser(propaganda)
    assert(res == propagandaFuncs)
  }
}
