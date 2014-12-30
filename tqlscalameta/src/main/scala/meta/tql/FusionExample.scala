package scala.meta.tql

/**
 * Created by Eric on 09.12.2014.
 */

import scala.language.reflectiveCalls
import ScalaMetaFusionTraverser._
import scala.meta.internal.ast._

object FusionExample extends App{

  val x = {
    import scala.meta._
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
  }
  var i = 0
  var j = 0
  var k = 0


  val getAllEven = down(collect{case Lit.Int(a) if a % 2 != 0 => {i += 1; println(i); a}}) map (_.map(_ * 10))
  val getAllEven2 = down(collect{case Lit.Int(a) if a % 2 != 0 => {k += 1; println(k); a}}) map (_.map(_ * 20))
  val getAllOdds = down(collect{case Lit.Int(a) if a % 2 != 0 => {j += 1; println(j); a}})

  val both = getAllOdds + getAllOdds + getAllEven + getAllEven2 + (getAllOdds + getAllOdds)
  println(both(x).result)
}
