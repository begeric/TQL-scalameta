package scala.meta.tql

/**
 * Created by Eric on 20.10.2014.
 */

import ScalaMetaTraverser2._
import scala.meta.internal.ast._
import scala.meta.syntactic.show._
import scala.language.reflectiveCalls

object Example extends App {


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

  val st = down(stateful2[List[scala.meta.Tree]](x => collect{
      case Term.If(a, b, c) => a
    })

  )
  //println(x.show[Raw])
  //val getAllInts = down(visit{case _ => println(x); List()})
  val getAllVals = (collect[Set]{case x: Defn.Val => x.pats.head.toString}).down

  val test = transform {
    case Lit.Int(a) => Lit.Int(a * 3)
    case Defn.Val(a, b, c, d) => Defn.Var(a,b,c,Some(d))
  }.down


  val t1: List[Int] = x.collect{case Lit.Int(a) if a > 10 => a}
  val t2: List[Int] = x.focus({case Term.If(_,_,_) => true}).down.collect{case Lit.Int(a) => a}
  val t3: (scala.meta.Tree, List[String]) = x.transform{case Defn.Val(a, b, c, d) => Defn.Var(a,b,c,Some(d)) andCollect(b.toString)}
  val t4: scala.meta.Tree = x.focus{case Lit.Int(a) => true}.transform{case x: Lit.Int => Lit.Int(1)}
  val t5: Set[String] = x.up.collect[Set]{case x: Defn.Val => x.pats.head.toString}
  val t6: List[Int] = x.focus({case Term.If(_,_,_) => true}).combine(down(collect{case Lit.Int(a) => a})).result
  val t7: scala.meta.Tree = x.transform {
    case Lit.Int(a) => Lit.Int(a * 3)
    case Defn.Val(a, b, c, d) => Defn.Var(a,b,c,Some(d))
  }

  val bfstest = bfs(collect{case Lit.Int(a) => a})
  val dfstest = down(collect{case Lit.Int(a) => a})
  //println(getAvg(x).result.map(_()))


  println(x.collect{case Lit.Int(a) => a})

  //println(bfstest(x))
  //println(t1)
  //println(t5)
  //println(bfstest(x).result)
  //println(dfstest(x).result)
}
