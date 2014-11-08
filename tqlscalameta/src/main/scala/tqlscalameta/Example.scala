package tqlscalameta

/**
 * Created by Eric on 20.10.2014.
 */

import ScalaMetaTraverser._
import tql._
import scala.meta._

import tqlscalameta._

import scala.reflect.ClassTag

object Example extends App{


  val x =
    q"""
       if (1 == 1) 1
       else 2
       5
       """

  println(x treeOf update{
    case _: Type => Type.Name("hey")
  })


}
