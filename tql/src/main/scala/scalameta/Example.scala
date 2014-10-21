package scalameta

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
       """

  /*val getAllIntLits = deep(collect[Tree, Int]{
    case Lit.Int(a) => println(a);a
  })                                               */

  //val changeAllIntLits = deep(update{case q"${_ : Int}" => q"17"})
  val changeAllIntLits = deep(update[Tree, Tree]{case Lit.Int(_) => q"117"})

  println(changeAllIntLits(x))
  println(show.ShowOps(x).show[syntactic.show.Raw])

  println(ScalaMetaTraverserHelper.build(Term.ApplyInfix) + ":2")

}
