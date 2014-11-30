package tools

import org.scalameter._
import org.scalameter.execution.LocalExecutor
import org.scalameter.reporting.LoggingReporter
import tools.ScalaToTree.CompilerProxy
import tools._
import tqlscalameta.ScalaMetaTraverser._
import scala.meta.syntactic.ast._

/**
 * Created by Eric on 20.10.2014.
 */


object CompleteBenchmark extends PerformanceTest {


	lazy val executor = LocalExecutor(
					new Executor.Warmer.Default, 
					Aggregator.min, 
					new Measurer.Default)
	lazy val reporter = new LoggingReporter
	lazy val persistor = Persistor.None

	val range = Gen.enumeration("size")(50)

	val compiler: CompilerProxy = ScalaToTree.loadCompiler
  val scalaTree = compiler.parseAndType(ScalaToTree.loadSource(System.getProperty("user.dir") + "/tqlscalameta/src/test/resources/Huffman.scala"))

  val scalaMetaTree:scala.meta.Tree = compiler.scalaToMeta(scalaTree)

	performance of "Variable name Collection" in {
		measure method "Scala Traverser" in {
      import compiler.compiler._

			using(range) in { j =>
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
			}
		}

		measure method "Scala Meta Traverser" in {
			using(range) in { j =>

        val result = new Traverser {
          var varNames = Set[String]()
          override def traverse(tree: scala.meta.Tree) = tree match {
            case _: Term.Name | _ : Lit.Char | _ : Lit.Int | _ : Type.Name | _: Term.Param | _: Type.Apply =>
            case Term.ApplyInfix(lhs, op, targs, args) =>
              traverse(lhs)
              traverse(op)
              targs.foreach(traverse(_))
              args.foreach(traverse(_))
            case Term.Select(qual, selector) =>
              traverse(qual)
              traverse(selector)
            case _ : Pat.Wildcard =>
            case Case(pat, conds, stats) =>
              traverse(pat)
              conds.foreach(traverse(_))
              stats.foreach(traverse(_))
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
		}

    measure method "TQL  CollectIn[Set]" in {
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
		}
	}
}