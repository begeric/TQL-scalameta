package tools

import org.scalatest.FunSuite
import tools.ScalaToTree.CompilerProxy
import scala.meta.internal.ast._

/**
 * Created by Eric on 10.12.2014.
 */
class FusionSuite extends FunSuite {

  val compiler: CompilerProxy = ScalaToTree.loadCompiler
  val scalaTree = compiler.parseAndType(ScalaToTree.loadSource(System.getProperty("user.dir") + "/tqlscalameta/src/test/resources/Huffman.scala"))

  val scalaMetaTree:scala.meta.Tree = compiler.scalaToMeta(scalaTree)


  test("Collect Lit.String(_)") {
    var result1: List[Int] = Nil
    var result2: List[Int] = Nil
    var result3: List[Int] = Nil

    {
      import tqlscalameta.ScalaMetaTraverser._
      val collectVals = down(collect{case Lit.Int(v) => v})
      val twice = collectVals + collectVals
      val fourtimes = twice + twice
      result1 = fourtimes(scalaMetaTree).result
    }

    {
      import tqlscalameta.ScalaMetaFusionTraverser._
      val collectVals = down(collect{case Lit.Int(v) => v})
      val twice = collectVals + collectVals
      val fourtimes = twice + twice
      result2 = fourtimes(scalaMetaTree).result
    }

    {
      import tqlscalameta.ScalaMetaFusionTraverser._
      val collectVals = down(optimize(collect{case Lit.Int(v) => v}))
      val twice = collectVals + collectVals
      val fourtimes = twice + twice
      result3 = fourtimes(scalaMetaTree).result
    }

    assert(result1.sorted == result2.sorted)
    assert(result1.sorted == result3.sorted)
  }
}