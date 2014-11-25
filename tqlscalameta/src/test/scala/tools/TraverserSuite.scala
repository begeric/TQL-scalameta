package tools
/**
 * Created by Eric on 20.10.2014.
 */

import org.scalatest._
import scala.collection.mutable.ListBuffer
import scala.meta.syntactic.ast._

class TraverserSuite extends FunSuite {
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

  test("Collect Lit.Int(_)") {
    val buffer = new ListBuffer[Int]()
    new Traverser {
      override def traverse(tree: scala.meta.Tree) = tree match {
        case Lit.Int(a) => buffer.append(a)
        case _ => super.traverse(tree)
      }
    }.traverse(code)
    assert(buffer.toSet == Set(5,3,17,1,2))
  }
}
