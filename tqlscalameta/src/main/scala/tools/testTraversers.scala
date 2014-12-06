package tools

import tools.ScalaToTree.CompilerProxy

import scala.meta.internal.ast._

/**
 * Created by Eric on 30.11.2014.
 */
object TestTraversers extends App {

  val compiler: CompilerProxy = ScalaToTree.loadCompiler
  val scalaTree = compiler.parseAndType(ScalaToTree.loadSource(System.getProperty("user.dir") + "/tqlscalameta/src/test/resources/Huffman.scala"))

  val scalaMetaTree:scala.meta.Tree = compiler.scalaToMeta(scalaTree)

  import compiler.compiler._

  var count1 = 0
  var count2 = 0
  /*val result1 = new compiler.compiler.Traverser {
    var varNames = Set[Int]()
    override def traverse(tree: Tree): Unit = tree match {
      case Literal(Constant(v: Int)) => varNames += v
      case _ => count1 += 1; super.traverse(tree)
    }

    def run(tree: Tree): Set[Int] = {
      varNames = Set[Int]()
      traverse(tree)
      varNames
    }
  }.run(scalaTree)*/
  println("pause")
  Thread.sleep(4000)
  println("starting")
  (0 until 100).foreach{i => 
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
  }

  (0 until 100).foreach{i => 
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
  }

  /*val map = new scala.collection.mutable.HashMap[String, Int]()
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

  val sorted = map.toList.sortBy(_._2).reverse.mkString("\n")*/


  //println(result1.toString + " : " + result2.toString)
  println(count1 + " : " + count2)

}
