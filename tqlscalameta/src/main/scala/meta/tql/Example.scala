package scala.meta.tql

/**
 * Created by Eric on 20.10.2014.
 */

import ScalaMetaTraverser._
import scala.meta.internal.ast._
import scala.meta.ui._
import scala.meta.syntactic._
import scala.language.reflectiveCalls
import scala.meta.dialects.Scala211

object Example extends App {


  val x =
    q"""
       val a = 5
       val c = 3
       c = 5
       if (3 == 17) {
        val c = 1
        while (a != c) {println(78)}
        val x = 14
        while (a != c) {println(85)}
       }
       else 2
       5
       """

  val tree =
    q"""
       val a = 5
       val c = 3
       c = 5
       if (3 == 17) {
        val c = {
          val d = "hey"
          22
        }
       }
       else "2"
       5
       """

  /*val getAllVals = (collect[Set]{case x: Defn.Val => x.pats.head.toString}).topDown

  val listToSetBool = topDown(transform{  //WithResult[Term.Apply, Term.Select, List[String]]
    case tt @ Term.Apply(t @ Term.Select(Term.Apply(Term.Name("List"), _), Term.Name("toSet")), _) =>
      t andCollect tt.toString
  })

  val test = transform {
    case Lit.Int(a) => Lit.Int(a * 3)
    case Defn.Val(a, b, c, d) => Defn.Var(a,b,c,Some(d))
  }.topDown

  val t1: List[Int] = x.collect{case Lit.Int(a) if a > 10 => a}
  val t2: List[Int] = x.focus({case Term.If(_,_,_) => true}).topDown.collect{case Lit.Int(a) => a}
  val t3: (scala.meta.Tree, List[String]) = x.transform{case Defn.Val(a, b, c, d) => Defn.Var(a,b,c,Some(d)) andCollect(b.toString)}
  val t4: scala.meta.Tree = tree.transform{case Lit.Int(x) => Lit.Int(x * 2)}
  val t5: Set[String] = x.bottomUp.collect[Set]{case x: Defn.Val => x.pats.head.toString}
  val t6: List[Int] = x.focus({case Term.If(_,_,_) => true}).combine(topDown(collect{case Lit.Int(a) => a})).result
  val t7: scala.meta.Tree = x.transform {
    case Lit.Int(a) => Lit.Int(a * 3)
    case Defn.Val(a, b, c, d) => Defn.Var(a,b,c,Some(d))
  }

  //println(t4)

  val hey = x \: focus{case _: Term.If => true} \: focus{case Lit.Int(x) => x > 2} \: collect{case Lit.Int(a) => a}

  val testUntil = until(collect{case Lit.Int(a) => a}, focus{case _:Term.While => true})

  val testAggregateUntil = tupledUntil(
    collect{case Lit.Int(a) => a},
    focus{case _:Term.While => true} ~> topDown(collect[Set]{case Lit.Int(a) => a * 2})
  )

  val fixtest = fix[List[Int]]{r =>
    collect{case Lit.Int(x) => x}
  }.topDown         */

  val yoyo = bottomUpBreak(collect{case x: Defn.Val => println(x);x.pats.head.toString})
  println(yoyo(tree))

  /*println(tree.collect{case Lit.Int(a) => a})
  println(tree.topDown.collect{case Lit.Int(a) => a}) //equiv to the above
  println(tree.topDownBreak.collect{case Lit.Int(a) => a})
  println(tree.bottomUp.collect{case Lit.Int(a) => a})
  println(tree.bottomUpBreak.collect{case Lit.Int(a) => a} )    */

}
