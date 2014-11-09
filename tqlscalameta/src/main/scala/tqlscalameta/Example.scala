package tqlscalameta

/**
 * Created by Eric on 20.10.2014.
 */

import ScalaMetaTraverser._
import scala.meta._
import scala.meta.syntactic.ast._

import tqlscalameta._

object Example extends App{


  val x =
    q"""
       if (3 == 17) 8
       else 2
       5
       """

  val getMin = down(stateful(Int.MaxValue){state =>
    visit{case Lit.Int(a) => (List(() => state), Math.min(state,a))}
  })

  println(getMin(x).result.map(_()))
}
