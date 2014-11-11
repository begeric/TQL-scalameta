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
       var a = 5
       var c = 3
       c = 5
       if (3 == 17) 8
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

  val find = downBreak(
    update{
      case  Defn.Val(mod, n, None, Term.Select(Term.Apply(Term.Name("List"), l), Term.Name("toSet")))=>
        Defn.Val(mod, n, None, Term.Apply(Term.Name("Set"), l))
    })

  val getAllInts = down(collectIn[Set]{case Lit.Int(a) => a})

  println(getAvg(x).result.map(_()))
  println(getAllInts(x).result)

}
