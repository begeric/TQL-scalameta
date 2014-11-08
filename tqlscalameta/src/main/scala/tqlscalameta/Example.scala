package tqlscalameta

/**
 * Created by Eric on 20.10.2014.
 */

import ScalaMetaTraverser._
import tql._
import scala.meta._
import scala.meta.syntactic.ast._

import tqlscalameta._

import scala.reflect.ClassTag

object Example extends App{


  val x =
    q"""
       if (1 == 1) 1
       else 2
       5
       """

  val getAllIntLits = downBreak(update{case Lit.Int(a) => Lit.Bool(false)})

  println(getAllIntLits(x).tree)


}
