package tools

import tools.ScalaToTree.CompilerProxy
import tools.Traverser

import scala.meta.syntactic.ast._

/**
 * Created by Eric on 30.11.2014.
 */
object TestTraversers extends App {

  val compiler: CompilerProxy = ScalaToTree.loadCompiler
  val scalaTree = compiler.parseAndType(ScalaToTree.loadSource(System.getProperty("user.dir") + "/tqlscalameta/src/test/resources/Huffman.scala"))

  val scalaMetaTree:scala.meta.Tree = compiler.scalaToMeta(scalaTree)

  import compiler.compiler._
  val result1 = new compiler.compiler.Traverser {
    var varNames = Set[String]()
    override def traverse(tree: Tree): Unit = tree match {
      case Literal(Constant(v: String)) => varNames += v
      case _ => super.traverse(tree)
    }

    def run(tree: Tree):Set[String] = {
      varNames = Set[String]()
      traverse(tree)
      varNames
    }
  }.run(scalaTree)

  val result2 = new tools.Traverser {
    var varNames = Set[String]()
    override def traverse(tree: scala.meta.Tree) = tree match {
      case Lit.String(v) => varNames += v
      case _ => super.traverse(tree)
    }

    def run(tree: scala.meta.Tree):Set[String] = {
      varNames = Set[String]()
      traverse(tree)
      varNames
    }
  }.run(scalaMetaTree)

  val map = new scala.collection.mutable.HashMap[String, Int]()
  new tools.Traverser {
    override def traverse(tree: scala.meta.Tree) = tree match {
      case _ =>
        if (map.contains(tree.getClass.toString))
          map(tree.getClass.toString) += 1
        else
          map += tree.getClass.toString -> 1
       super.traverse(tree)
    }
  }.traverse(scalaMetaTree)

  val sorted = map.toList.sortBy(_._2).reverse.mkString("\n")

  println(sorted)

  //println(result1.toString + " : " + result2.toString)

}
