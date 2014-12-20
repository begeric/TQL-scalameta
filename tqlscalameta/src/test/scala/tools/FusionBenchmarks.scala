package tools

import org.scalameter.reporting.LoggingReporter
import org.scalameter._
import org.scalameter.execution.LocalExecutor
import tools.ScalaToTree.CompilerProxy
import scala.meta.internal.ast._

/**
 * Created by Eric on 10.12.2014.
 */
object FusionBenchmarks  extends PerformanceTest {

  lazy val executor = LocalExecutor(
    new Executor.Warmer.Default,
    Aggregator.min,
    new Measurer.Default)
  lazy val reporter = new LoggingReporter
  lazy val persistor = Persistor.None

  val range = Gen.enumeration("size")(100)

  val compiler: CompilerProxy = ScalaToTree.loadCompiler
  val scalaTree = compiler.parseAndType(ScalaToTree.loadSource(System.getProperty("user.dir") + "/tqlscalameta/src/test/resources/Huffman.scala"))

  val scalaMetaTree:scala.meta.Tree = compiler.scalaToMeta(scalaTree)


  performance of "Variable name Collection" in {
    measure method "ScalaMetaTraverser" in {
      import scala.meta.tqlscalameta.ScalaMetaTraverser._
      using(range) in { j =>
        val collectVals = down(collect{case Lit.String(v) => v})
        val twice = collectVals + collectVals
        val fourtimes = twice + twice
        fourtimes(scalaMetaTree).result
      }
    }

    measure method "ScalaMetaFusionTraverser" in {
      import scala.meta.tqlscalameta.ScalaMetaFusionTraverser._
      using(range) in { j =>
        val collectVals = down(collect{case Lit.String(v) => v})
        val twice = collectVals + collectVals
        val fourtimes = twice + twice
        fourtimes(scalaMetaTree).result
      }
    }

    measure method "ScalaMetaFusionTraverser with optimize" in {
      import scala.meta.tqlscalameta.ScalaMetaFusionTraverser._
      using(range) in { j =>
        val collectVals = down(optimize(collect{case Lit.String(v) => v}))
        val twice = collectVals + collectVals
        val fourtimes = twice + twice
        fourtimes(scalaMetaTree).result
      }
    }
  }

}
