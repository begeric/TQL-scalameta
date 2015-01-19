package tools
/**
 * Created by Eric on 20.10.2014.
 */

import org.scalatest._
import scala.collection.mutable.ListBuffer
import scala.meta.internal.ast._
import scala.meta.ui._
import scala.meta.syntactic._
import scala.meta.dialects.Scala211


class TransformerSuite extends FunSuite {
  val code = {
    import scala.meta._
    q"""
       val a = 5
       val c = 3
       if (3 == 17) {
        val c = 1
       }
       else 2
       5
       """
  }

  val finalCode =  {
    import scala.meta._
    q"""
       val a = 1
       val c = 1
       if (1 == 1) {
        val c = 1
       }
       else 1
       1
       """
  }

  test("Change Lit.Int(_) to Lit.Int(1)") {
    val buffer = new ListBuffer[Int]()
    val transformedCode = new Transformer {
      override def transform(tree: scala.meta.Tree) = tree match {
        case Lit.Int(a) => Lit.Int(1)
        case _ => super.transform(tree)
      }
    }.transform(code)
    assert(transformedCode.toString == finalCode.toString)
  }
}
