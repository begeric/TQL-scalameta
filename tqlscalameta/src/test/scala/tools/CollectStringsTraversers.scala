package tools

import tools.ScalaToTree.CompilerProxy

import scala.meta.internal.ast.Lit

/**
 * Created by Eric on 06.12.2014.
 */
object CollectStringsTraversers {

  def scala(compiler: CompilerProxy) = new  compiler.compiler.Traverser {
    import compiler.compiler._
    var varNames = Set[String]()
    override def traverse(tree: Tree): Unit = tree match {
      case Literal(Constant(v: String)) => varNames += v
      case _ => super.traverse(tree)
    }

    def apply(tree: Tree):Set[String] = {
      varNames = Set[String]()
      traverse(tree)
      varNames
    }
  }

  val scalameta = new TraverserTableTag {
    var varNames = Set[String]()
    override def traverse(tree: scala.meta.Tree) = tree match {
      case Lit.String(v) => varNames += v
      case _ => super.traverse(tree)
    }

    def apply(tree: scala.meta.Tree):Set[String] = {
      varNames = Set[String]()
      traverse(tree)
      varNames
    }
  }

  val scalametaOptimzed = new tools.OptimzedOrderTraverser {
    var varNames = Set[String]()

    override def traverse(tree: meta.Tree) = tree match {
      case Lit.String(v) => varNames += v
      case _ => super.traverse(tree)
    }

    def apply(tree: meta.Tree): Set[String] = {
      varNames = Set[String]()
      traverse(tree)
      varNames
    }
  }
}
