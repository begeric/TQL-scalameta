package tools

import org.scalameter._
import org.scalameter.execution.LocalExecutor
import org.scalameter.reporting.LoggingReporter
import tools.ScalaToTree.CompilerProxy
import tools._
import tqlscalameta.ScalaMetaTraverser._
import scala.meta
import scala.meta.internal.ast._

/**
 * Created by Eric on 20.10.2014.
 */


object CompareBenchmarks extends PerformanceTest {

	lazy val executor = LocalExecutor(
					new Executor.Warmer.Default, 
					Aggregator.min, 
					new Measurer.Default)
	lazy val reporter = new LoggingReporter
	lazy val persistor = Persistor.None

	val range = Gen.enumeration("size")(100)
  val maxIters = 1

	val compiler: CompilerProxy = ScalaToTree.loadCompiler
  val scalaTree = compiler.parseAndType(ScalaToTree.loadSource(System.getProperty("user.dir") + "/tqlscalameta/src/test/resources/GenSeqLike.scala"))

  val scalaMetaTree:scala.meta.Tree = compiler.scalaToMeta(scalaTree)

	performance of "Variable name Collection" in {
		measure method "Scala Traverser" in {
      import compiler.compiler._

			using(range) in { j =>
        var i = 0
        while (i < maxIters){
  				val result = new compiler.compiler.Traverser {
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
          i += 1
        }
			}
		}

		measure method "Scala Meta Traverser" in {
			using(range) in { j =>
        var i = 0
        while (i < maxIters) {
          val result = new tools.OptimzedOrderTraverser {
            var varNames = Set[String]()

            override def traverse(tree: meta.Tree) = tree match {
              case Lit.String(v) => varNames += v
              case _ => super.traverse(tree)
            }

            def run(tree: meta.Tree): Set[String] = {
              varNames = Set[String]()
              traverse(tree)
              varNames
            }
          }.run(scalaMetaTree)
          i += 1
        }
			}
		}

    measure method "Scala Meta Traverser Optimized" in {
      using(range) in { j =>
        var i = 0
        while (i < maxIters){
          val result = new TraverserTableTag {
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
          i += 1
        }
      }
    }

    /*measure method "TQL  CollectIn[Set]" in {
      using(range) in { j =>
        def collectVals = down(collectIn[Set]{case Lit.String(v) => v})
        collectVals(scalaMetaTree)
      }
    }

    measure method "TQL CollectionLikeUI Collect" in {
      using(range) in { j =>
        scalaMetaTree.collect{case Lit.String(v) => v}.toSet
      }
    }

		measure method "TQL CollectionLikeUI CollectIn[Set]" in {
			using(range) in { j =>
        val result = scalaMetaTree.collectIn[Set]{case Lit.String(v) => v}
			}
		}  */
	}
}