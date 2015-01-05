package tools

import org.scalameter.reporting.LoggingReporter
import org.scalameter._
import org.scalameter.execution.LocalExecutor
import tools.ScalaToTree.CompilerProxy
import scala.meta.internal.ast._

import scala.meta.internal.ast.Lit

/**
 * Created by Eric on 05.01.2015.
 */
object OptimizeBenchmarks extends PerformanceTest{
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
      import scala.meta.tql.ScalaMetaTraverser._
      using(range) in { j =>
        val collectStrings = down(collect{case Lit.String(v) => v})
        val collectInts = down(collect{case Lit.Int(v) => v.toString})
        val collectVals = down(collect{case v: Decl.Val => "val"})
        val twice = collectStrings + collectInts + collectVals
        val fourtimes = twice + twice + twice
        fourtimes(scalaMetaTree).result
      }
    }

    measure method "ScalaMetaFusionTraverser" in {
      import scala.meta.tql.ScalaMetaFusionTraverser._
      using(range) in { j =>
        val collectStrings = down(collect{case Lit.String(v) => v})
        val collectInts = down(collect{case Lit.Int(v) => v.toString})
        val collectVals = down(collect{case v: Decl.Val => "val"})
        val twice = collectStrings + collectInts + collectVals
        val fourtimes = twice + twice + twice
        fourtimes(scalaMetaTree).result
      }
    }

    measure method "ScalaMetaFusionTraverser with optimize" in {
      import scala.meta.tql.ScalaMetaFusionTraverser2._
      using(range) in { j =>
        val collectStrings = down(optimize(collect{case Lit.String(v) => v}))
        val collectInts = down(optimize(collect{case Lit.Int(v) => v.toString}))
        val collectVals = down(optimize(collect{case v: Decl.Val => "val"}))
        val twice = collectStrings + collectInts + collectVals
        val fourtimes = twice + twice + twice
        fourtimes(scalaMetaTree).result
      }
    }
  }
}
