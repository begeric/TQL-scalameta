package meta.tql

import org.scalatest.FunSuite
import tools.ScalaToTree.CompilerProxy
import tools.{ScalaToTree, Traverser}

import scala.meta.internal.ast.{Term, Defn}
import scala.meta.tql.ScalaMetaTraverser._

/**
 * Created by Eric on 19.01.2015.
 */
class ForbidVarsInMethodsSuite extends FunSuite {

  val compiler: CompilerProxy = ScalaToTree.loadCompiler

  val propaganda = ScalaToTree.load(System.getProperty("user.dir") + "/tqlscalameta/src/test/resources/Propaganda.scala")
  val propagandaVars = List.empty[String]

  //tql traverser
  val forbidVarsInMethods = {
    def checkMethod: Matcher[List[String]] =
      focus{ case _: Defn.Procedure => true} feed { method =>

        val warn = collect{
          case Defn.Var(_, (b: Term.Name):: Nil,_, _) =>
            b + " should not be defined in " + method.name
        }

        def rule: Matcher[List[String]] =
          checkMethod | warn + rule.children
        rule.children
      }
    checkMethod.topDownBreak
  }

  //'notmal' traverser
  def forbidVarsInMethods2(tree:  scala.meta.Tree) = {
    import scala.meta.internal.ast._
    var warnings = List.empty[String]
    var currentMethod: Term.Name = null
    new Traverser {
      override def traverse(tree:  scala.meta.Tree): Unit = tree match {
        case f: Defn.Procedure =>
          val oldFunc = currentMethod
          currentMethod = f.name
          super.traverse(tree)
          currentMethod = oldFunc
        case Defn.Var(_, (b: Term.Name):: Nil,_, _)
          if currentMethod != null =>
          val newWarning = b + " should not be used in " + currentMethod.name
          warnings = newWarning :: warnings
          super.traverse(tree)
        case _ =>
          super.traverse(tree)
      }
    }.traverse(tree)
    warnings
  }

  //tests

  test("forbidVarsInMethods") {
    val res = forbidVarsInMethods(propaganda).result
    assert(res == propagandaVars)
  }

  test("forbidVarsInMethods2") {
    val res = forbidVarsInMethods2(propaganda)
    assert(res == propagandaVars)
  }
}
