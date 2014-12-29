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


  val getAllPairs =  down(collect[Set]{case Lit.Int(a) if a % 2 != 0 => a})
  val getAllIntLT5 = getAllPairs feed {res => down(collect[Set]{case Lit.Int(a) if a < 5 && res.contains(a) => a})}
  val getAllIntGT5 = down(collect[Set]{case Lit.Int(a) if a > 5 => a})

  val both = getAllIntLT5 + getAllIntGT5

  println(both(x).result)
}
