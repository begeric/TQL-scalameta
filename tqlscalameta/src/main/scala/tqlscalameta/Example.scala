package tqlscalameta

/**
 * Created by Eric on 20.10.2014.
 */

import ScalaMetaTraverser._
import tql._
import scala.meta._
import CombinatorsSugar._
import Monoids._
import AllowedTransformationsMacros._

object Example extends App{


  val x =
    q"""
       if (1 == 1) 1
       else 2
       5
       """

  val getAllIntLits = downBreak(collect{case Lit.Int(a) => 2})
  val getAllIntInts = downBreak(
    collect{case Lit.Int(a) if a > 1  => 1}.map((_, Nil)) +
    collect{case Lit.Int(a) if a <= 1 => 2}.map((Nil, _))
  )

  val changeAllIntLits = downBreak(update{case _: Lit.Int => q"22"})

  println(getAllIntInts(x))
  println(getAllIntLits(x))
  println(changeAllIntLits(x))

}
