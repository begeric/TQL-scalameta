package scalameta

import scala.meta.Lit

/**
 * Created by Eric on 20.10.2014.
 */
object Example extends App{

  import tql._
  import scala.meta._
  import syntactic._
  import ScalaMetaTraverser._
  import Monoids._

  def showTree(x: Tree) = show.ShowOps(x).show[syntactic.show.Raw]

  val x =
    q"""
       if (1 == 1) 1
       else 2
       5
       """

  val getAllIntLits = deep(collect{case Lit.Int(a) => 1})
  println(x)

  //val changeAllIntLits = deep(update{case q"${_ : Int}" => q"17"})
  val changeAllIntLits = deep(update[Tree, Tree]{case Lit.Int(_) => q"117"})

  println(getAllIntLits(x))

  println(x \\ (filter[Tree]{case Term.If(_,_,_) => true} \\ update[Tree, Tree]{case Lit.Int(_) => q"118"}))

  println(ScalaMetaTraverserHelper.build(Term.ApplyInfix) + "")

}
