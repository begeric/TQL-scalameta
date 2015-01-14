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


  val x = {
    import scala.meta._
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
  }

  val getMin = topDown(stateful(Int.MaxValue){state =>
    visit{case Lit.Int(a) => (List(() => state), Math.min(state,a))}
  })


  val getAvg = topDown(stateful((0, 0)){state => {
      lazy val avg = state._1 / state._2
      visit{case Lit.Int(a) => (List(() => avg), (state._1 + a, state._2 + 1))}
    }
  })

  val st = topDown(stateful2[List[scala.meta.Tree]](x => collect{
      case Term.If(a, b, c) => a
    })

  )
  //println(x.show[Raw])
  //val getAllInts = topDown(visit{case _ => println(x); List()})
  val getAllVals = (collect[Set]{case x: Defn.Val => x.pats.head.toString}).topDown

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
  val t4: scala.meta.Tree = x.focus{case Lit.Int(a) => true}.transform{case x: Lit.Int => Lit.Int(1)}
  val t5: Set[String] = x.bottomUp.collect[Set]{case x: Defn.Val => x.pats.head.toString}
  val t6: List[Int] = x.focus({case Term.If(_,_,_) => true}).combine(topDown(collect{case Lit.Int(a) => a})).result
  val t7: scala.meta.Tree = x.transform {
    case Lit.Int(a) => Lit.Int(a * 3)
    case Defn.Val(a, b, c, d) => Defn.Var(a,b,c,Some(d))
  }

  val bfstest = bfs(collect{case Lit.Int(a) => a})
  val dfstest = topDown(collect{case Lit.Int(a) => a})
  //println(getAvg(x).result.map(_()))
          // \: (focus{case _: Lit => true}
  /*val a = focus{case _: Lit => true} \ collect{case Lit.Int(a) => a}
  val b = focus{case _: Term.If => true} \: a*/

    /*implicit class Test(x: Int){
      def :\(y: Int) = ???
      def \:(y: Int) = ???
    }
  tql.scalametaMacros.showAST(5 :\ 6)  */

  //val hey = x \: (focus{case _: Term.If => true} \: focus{case _: Lit => true} \: collect{case Lit.Int(a) => a})
  val hey = x \: focus{case _: Term.If => true} \: focus{case Lit.Int(x) => x > 2} \: collect{case Lit.Int(a) => a}

  //println(hey)

  val testUntil = until(collect{case Lit.Int(a) => a}, focus{case _:Term.While => true})

  val testAggregateUntil = tupledUntil(
    collect{case Lit.Int(a) => a},
    focus{case _:Term.While => true} ~> topDown(collect[Set]{case Lit.Int(a) => a * 2})
  )

  val fixtest = fix[List[Int]]{r =>
    collect{case Lit.Int(x) => x}
  }.topDown

  val sdfgjh = transform{
    case _: Defn.Val => (Type.Name("hdey"), tql.Monoid.Void)
  }
  println(sdfgjh(x))
  //println(fixtest(x))
  //tql.scalametaMacros.showAST(5 \: 6)
  //println(hey)
  //println(bfstest(x))
  //println(t1)
  //println(t5)
  //println(bfstest(x).result)
  //println(dfstest(x).result)
}
