package examples

/**
 * Created by Eric on 09.12.2014.
 */

import scala.language.reflectiveCalls
import scala.meta.tql.ScalaMetaFusionTraverser._
import scala.meta.internal.ast._
import scala.meta.ui._
import scala.meta.syntactic._
import scala.meta.dialects.Scala211

object FusionExample extends App{

  val x =
    q"""
       val a = 1
       val c = 3
       c = 5
       if (3 == 17) {
        val c = 1
       }
       else 2
       5
    """
  var i = 0
  var j = 0
  var k = 0


  val getAllEven = topDown(collect{case Lit.Int(a) if a % 2 != 0 => {i += 1; println(i); a}}) map (_.map(_ * 10))
  val getAllEven2 = topDown(collect{case Lit.Int(a) if a % 2 != 0 => {k += 1; println(k); a}}) map (_.map(_ * 20))
  val getAllOdds = topDown(collect{case Lit.Int(a) if a % 2 != 0 => {j += 1; println(j); a}})

  val both = getAllOdds + getAllOdds + getAllEven + getAllEven2 + (getAllOdds + getAllOdds)
  println(both(x).result)
}
