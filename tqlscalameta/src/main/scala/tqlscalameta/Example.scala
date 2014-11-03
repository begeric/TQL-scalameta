package tqlscalameta

import scala.meta.Lit

/**
 * Created by Eric on 20.10.2014.
 */
object Example extends App{

  import tql._
  import scala.meta._
  import syntactic._
  import ScalaMetaTraverser._
  import CombinatorsSugar._
  import Monoids._
  import AllowedTransformationsMacros._

  val x =
    q"""
       if (1 == 1) 1
       else 2
       5
       """


  val getAllIntLits = downBreak(collect{case Lit.Int(a) => 2})

  val changeAllIntLits = downBreak(update{case _: Lit.Int => q"18"})

  println(getAllIntLits(x))
  println(changeAllIntLits(x))

}
