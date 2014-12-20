package tools

import org.scalameter._
import org.scalameter.execution.LocalExecutor
import org.scalameter.reporting.LoggingReporter
import tools.ScalaToTree.CompilerProxy
import scala.language.reflectiveCalls

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
		measure method "Scala Traverser" in {
			using(range) in { j =>
        CollectStringsTraversers.scalaTraverser(compiler).apply(scalaTree)
        import org.scalameta.reflection.Metadata
			}
		}
    measure method "Hand written Scala Meta Traverser" in {
      using(range) in { j =>
        CollectStringsTraversers.scalametaHandwritten(scalaMetaTree)
      }
    }

    /*measure method "Scala Meta Traverser" in {
      using(range) in { j =>
        val result = CollectStringsTraversers.scalametaOptimzedTraverser(scalaMetaTree)
      }
    }

    measure method "Scala Meta Traverser Optimized" in {
      using(range) in { j =>
        val result = CollectStringsTraversers.scalametaTraverser(scalaMetaTree)
      }
    } */

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