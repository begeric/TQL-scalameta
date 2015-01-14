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
      import scala.meta.tql.ScalaMetaTraverser._
      using(range) in { j =>
        val collectVals = topDown(collect{case Lit.String(v) => v})
        val twice = collectVals// + collectVals + collectVals + collectVals
        val fourtimes = twice + twice
        fourtimes(scalaMetaTree).result
      }
    }

    measure method "ScalaMetaFusionTraverser" in {
      import scala.meta.tql.ScalaMetaFusionTraverser._
      using(range) in { j =>
        val collectVals = topDown(collect{case Lit.String(v) => v})
        val twice = collectVals// + collectVals + collectVals + collectVals
        val fourtimes = twice + twice
        fourtimes(scalaMetaTree).result
      }
    }

    measure method "ScalaMetaFusionTraverser with optimize" in {
      import scala.meta.tql.ScalaMetaFusionTraverser._
      using(range) in { j =>
        val collectVals = topDown(optimize(collect{case Lit.String(v) => v}))
        val twice = collectVals// + collectVals
        val fourtimes = twice + twice
        fourtimes(scalaMetaTree).result
      }
    }
  }

 /* performance of "Fused map" in {
    measure method "ScalaMetaTraverser" in {
      import scala.meta.tql.ScalaMetaTraverser._
      using(range) in { j =>
        val getAllEven = topDown(collect{case Lit.Int(a) if a % 2 != 0 => a}) map (_.map(_ * 10))
        val getAllEven2 = topDown(collect{case Lit.Int(a) if a % 2 != 0 => a}) map (_.map(_ * 20))
        val getAllOdds = topDown(collect{case Lit.Int(a) if a % 2 != 0 => a})

        val all = getAllEven + getAllEven2 + getAllOdds + getAllOdds
        all(scalaMetaTree)
      }
    }

    measure method "ScalaMetaFusionTraverser" in {
      import scala.meta.tql.ScalaMetaFusionTraverser._
      using(range) in { j =>
        val getAllEven = topDown(collect{case Lit.Int(a) if a % 2 != 0 => a}) map (_.map(_ * 10))
        val getAllEven2 = topDown(collect{case Lit.Int(a) if a % 2 != 0 => a}) map (_.map(_ * 20))
        val getAllOdds = topDown(collect{case Lit.Int(a) if a % 2 != 0 => a})

        val all = getAllEven + getAllEven2 + getAllOdds + getAllOdds
        all(scalaMetaTree)
      }
    }

    measure method "ScalaMetaFusionTraverser2" in {
      import scala.meta.tql.ScalaMetaFusionTraverser._
      using(range) in { j =>
        val getAllEven = topDown(collect{case Lit.Int(a) if a % 2 != 0 => a}) map (_.map(_ * 10))
        val getAllEven2 = topDown(collect{case Lit.Int(a) if a % 2 != 0 => a}) map (_.map(_ * 20))
        val getAllOdds = topDown(collect{case Lit.Int(a) if a % 2 != 0 => a})

        val all = getAllEven + getAllEven2 + (getAllOdds + getAllOdds)
        all(scalaMetaTree)
      }
    }
  }

  performance of "Fused feed" in {

    measure method "ScalaMetaTraverser" in {
      import scala.meta.tql.ScalaMetaTraverser._
      using(range) in { j =>
        val getAllPairs = topDown(collect[Set] { case Lit.Int(a) if a % 2 != 0 => a})
        val getAllIntLT5 = getAllPairs feed { res => topDown(collect[Set] { case Lit.Int(a) if a < 5 && res.contains(a) => a})}
        val getAllIntGT5 = topDown(collect[Set] { case Lit.Int(a) if a > 5 => a})

        val both = getAllIntGT5 + getAllIntLT5 + getAllIntGT5
        both(scalaMetaTree)
      }
    }

    measure method "ScalaMetaFusionTraverser" in {
      import scala.meta.tql.ScalaMetaFusionTraverser._
      using(range) in { j =>
        val getAllPairs =  topDown(collect[Set]{case Lit.Int(a) if a % 2 != 0 => a})
        val getAllIntLT5 = getAllPairs feed {res => topDown(collect[Set]{case Lit.Int(a) if a < 5 && res.contains(a) => a})}
        val getAllIntGT5 = topDown(collect[Set]{case Lit.Int(a) if a > 5 => a})

        val both = getAllIntGT5 + getAllIntLT5 + getAllIntGT5
        both(scalaMetaTree)
      }
    }

    measure method "ScalaMetaFusionTraverser reverse order" in {
      import scala.meta.tql.ScalaMetaFusionTraverser._
      using(range) in { j =>
        val getAllPairs =  topDown(collect[Set]{case Lit.Int(a) if a % 2 != 0 => a})
        val getAllIntLT5 = getAllPairs feed {res => topDown(collect[Set]{case Lit.Int(a) if a < 5 && res.contains(a) => a})}
        val getAllIntGT5 = topDown(collect[Set]{case Lit.Int(a) if a > 5 => a})

        val both = getAllIntLT5 + getAllIntGT5 + getAllIntGT5
        both(scalaMetaTree)
      }
    }
  }  */


}
