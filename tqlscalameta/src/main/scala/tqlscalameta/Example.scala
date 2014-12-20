package tqlscalameta

/**
 * Created by Eric on 20.10.2014.
 */

import ScalaMetaFusionTraverser._
import scala.meta.internal.ast._
import scala.meta.syntactic.show._
import scala.language.reflectiveCalls

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

  //println(x.show[Raw])
  //val getAllInts = down(visit{case _ => println(x); List()})
  val getAllVals = (collect{case x: Defn.Val => x.pats.head.toString}).down

  val test = transform {
    case Lit.Int(a) => Lit.Int(a * 3) withResult List(a)
  }.down

  /*val t1 = x.collect{case Lit.Int(a) if a > 10 => a}
  val t2 = x.filter({case Term.If(_,_,_) => true}).down.collect{case Lit.Int(a) => a}
  val t3 = x.transform{case Defn.Val(a, b, c, d) => Defn.Var(a,b,c,Some(d)) andCollect(b)}
  val t4 = x.filter{case Lit.Int(a) => true}.transform{case x: Lit.Int => Lit.Int(1)}
  val t5 = x.up.collectIn[Set]{case x: Defn.Val => x.pats.head.toString}
  val t6 = x.filter({case Term.If(_,_,_) => true}).combine(down(collect{case Lit.Int(a) => a})).result */

  //val bfstest = bfs(collect{case Lit.Int(a) => a})

  //println(bfstest(x))
  /*println(t5)
  println(getAvg(x).result.map(_()))*/
  println(getAllVals(x).result)

  //test[Tree]{case x @ Term.If(Lit.Int(a), b, c) => true}

  /*object Test {
    trait Stuff[T] {
      type R
      def zero: R
    }
    object Stuff{
      implicit def nothing = new Stuff[Nothing]{
        type R = List[Int]
        def zero = List(1,2,3)
      }

      implicit def others[A[_] <: Traversable[_]] = new Stuff[A[_]] {
        type R = A[Int]
        def zero = Traversable.
      }
    }
    def test[T[_]](implicit ev: Stuff[T[_]]): ev.R = ev.zero

    println(test)
  }
  Test.test  */
}
