package tqlscalameta

/**
 * Created by Eric on 20.10.2014.
 */

import ScalaMetaTraverser._
import scala.meta.syntactic.ast._

object Example extends App{


  val x = {
    import scala.meta._
    q"""
       val a = 5
       val c = 3
       c = 5
       if (3 == 17) {
        val c = 1
       }
       else 2
       5
       """
  }

  val getMin = down(stateful(Int.MaxValue){state =>
    visit{case Lit.Int(a) => (List(() => state), Math.min(state,a))}
  })


  val getAvg = down(stateful((0, 0)){state => {
      lazy val avg = state._1 / state._2
      visit{case Lit.Int(a) => (List(() => avg), (state._1 + a, state._2 + 1))}
    }
  })

  val getAllInts = down(collectIn[Set]{case Lit.Int(a) => a})
  val getAllVals = down(collectIn[Set]{case x: Defn.Val => x.pats.head.toString})

  val t1 = x.collect{case Lit.Int(a) if a > 10 => a}.result
  val t2 = x.filter({case Term.If(_,_,_) => true}).down.collect{case Lit.Int(a) => a}.result
  val t3 = x.update{case Defn.Val(a, b, c, d) => Defn.Var(a,b,c,Some(d))}.tree
  val t4 = x.filter{case Lit.Int(a) => true}.update{case x: Lit.Int => Lit.Int(1)}.tree
  val t5 = x.collectIn[Set]{case x: Defn.Val => x.pats.head.toString}.result
  val t6 = x.filter({case Term.If(_,_,_) => true}).combine(down(collect{case Lit.Int(a) => a})).result

  println(t4)
  println(getAvg(x).result.map(_()))
  println(getAllVals(x).result)
}
