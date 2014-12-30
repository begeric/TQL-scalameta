package tools

import org.scalameter._
import org.scalameter.execution.LocalExecutor
import org.scalameter.reporting.LoggingReporter
import tools.ScalaToTree.CompilerProxy
import scala.language.reflectiveCalls
import scala.meta

/**
 * Created by Eric on 20.10.2014.
 */


object CompareBenchmarks extends PerformanceTest {

	lazy val executor = LocalExecutor(
					new Executor.Warmer.Default,
					Aggregator.average,
					new Measurer.Default)
	lazy val reporter = new LoggingReporter
	lazy val persistor = Persistor.None

	val range = Gen.enumeration("size")(100)

	val compiler: CompilerProxy = ScalaToTree.loadCompiler
  val scalaTree = compiler.parseAndType(ScalaToTree.loadSource(System.getProperty("user.dir") + "/tqlscalameta/src/test/resources/Huffman.scala"))

  val scalaMetaTree:scala.meta.Tree = compiler.scalaToMeta(scalaTree)

	performance of "Variable name Collection" in {
		/*measure method "Scala Traverser" in {
			using(range) in { j =>
        CollectStringsTraversers.scalaTraverser(compiler).apply(scalaTree)
			}
		}

    measure method "Basic Scala Meta Traverser" in {
      using(range) in { j =>
        CollectStringsTraversers.basicscalametaTraverser(scalaMetaTree)
      }
    }

    measure method "Hand written Scala Meta Traverser" in {
      using(range) in { j =>
        CollectStringsTraversers.scalametaHandwritten(scalaMetaTree)
      }
    }

    measure method "Scala Meta Traverser" in {
      using(range) in { j =>
        CollectStringsTraversers.scalametaTraverser(scalaMetaTree)
      }
    } */

    measure method "Scala Meta Traverser" in {
      using(range) in { j =>
        new Transformer {
          var varNames = Set[String]()
          import scala.meta.internal.ast._
          override def transform(tree: meta.Tree) = tree match {
            case x @ Lit.String(v) =>
              varNames += v
              x
            case _ => super.transform(tree)
          }

          def apply(tree: meta.Tree):Set[String] = {
            varNames = Set[String]()
            transform(tree)
            varNames
          }
        }.apply(scalaMetaTree)
      }
    }

    import scala.meta.internal.ast._

    measure method "TQL  CollectIn[Set] ScalaMetaTraverser" in {
      import scala.meta.tql.ScalaMetaTraverser._
      val collectVals = down(collect[Set]{case Lit.String(v) => v})
      using(range) in { j =>
        collectVals(scalaMetaTree)
      }
    }

    measure method "TQL  CollectIn[Set] ScalaMetaTraverser2" in {
      import scala.meta.tql.ScalaMetaTraverser2._
      val collectVals = down(collect[Set]{case Lit.String(v) => v})
      using(range) in { j =>
        collectVals(scalaMetaTree)
      }
    }
    /*
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