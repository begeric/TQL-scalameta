package tqlscalameta

/**
 * Created by Eric on 20.10.2014.
 */

import ScalaMetaTraverser._
import tql._
import scala.meta._
import CombinatorsSugar._

import tqlscalameta._

object Example extends App{


  val x =
    q"""
       if (1 == 1) 1
       else 2
       5
       """
  val y =
    q"""
       1
       2
       3
       """

  val getAllIntLits = downBreak(collect{case Lit.Int(a) => 2})
  val getAllIntInts = downBreak(
    collect{case Lit.Int(a) if a > 1  => 1} ~
    collect{case Lit.Int(a) if a <= 1 => 3}
  )

  val getAllInts2 = downBreak(collect{case Lit.Int(a) if a > 1  => a}) feed { res =>
    downBreak(collect{case Lit.Int(a) if a < 5 && res.contains(a) => a})
  }

  val changeAllIntLits = downBreak(update{case _: Lit.Int => q"22"})

  val LitsInsideBlock: TreeMapper[List[Lit]] = {
    val allBlocks = guard[Term.Block]{case t: Term.Block => true}
    val blockVals = allBlocks \ collect{case l: Lit => l}
    blockVals.down | allBlocks \ LitsInsideBlock
  }

  println(x treeOf down(collect{case l: Lit => l}) ~> down(transform[Lit, Lit]{case l: Lit => Lit.Bool(true)}))

  println(LitsInsideBlock(y).result)
  println(getAllIntInts(x).result)
  println(getAllInts2(x).result)
  println(getAllIntLits(x).result)
  println(changeAllIntLits(x).tree)

}
