package tools
/**
 * Created by Eric on 25.11.2014.
 */

import java.net.URLClassLoader
import java.lang.reflect.Method
import scala.tools.reflect.FrontEnd
import java.io._
import scala.reflect.runtime.{universe => ru}
import scala.tools.reflect.{ToolBox, ToolBoxError}
import scala.compat.Platform.EOL
import scala.meta.internal.hosts.scalac.Scalahost
//import scala.meta.semantic.{Host => PalladiumHost}
//import scala.meta.internal.hosts.scalac.{Host => OurHost}
import scala.meta.internal.hosts.scalac.{Scalahost  => OurHost}

/*
* Lame attempt to make available the scala compiler to the user, to make use of scala and scala.meta Trees
* Most of the code have been taken from here: https://github.com/scalameta/scalahost/blob/master/tests/src/test/scala/ScalaToMeta.scala
* */
object ScalaToTree {

	trait CompilerProxy {
		type Compiler <: scala.tools.nsc.Global
		val compiler: Compiler
		val tb: ToolBox[ru.type]

    import compiler._
    import analyzer._

    var m_frontEnd: Method = null
    var frontEnd: FrontEnd = null

    /*Awesomely awful. If you know how to have early initialization with anonymous class please tell me
    * Please have mercy on me
    * */
    def init: Unit ={
      m_frontEnd = tb.getClass.getDeclaredMethod("frontEnd")
      frontEnd = m_frontEnd.invoke(tb).asInstanceOf[scala.tools.reflect.FrontEnd]
    }

    private def throwIfErrors(): Unit = if (frontEnd.hasErrors)
      throw ToolBoxError("reflective compilation has failed:" + EOL + EOL + (frontEnd.infos map (_.msg) mkString EOL))

    def parseAndType(code: String): compiler.Tree = {
      reporter.reset()
      frontEnd.reset()

      val run = new compiler.Run
      phase = run.parserPhase
      globalPhase = run.parserPhase
      val unit = compiler.newCompilationUnit(code, "<memory>")
      unit.body = compiler.newUnitParser(unit).parse()
      throwIfErrors()
      phase = run.namerPhase
      globalPhase = run.namerPhase
      newNamer(rootContext(unit)).enterSym(unit.body)
      throwIfErrors()

      phase = run.phaseNamed("packageobjects")
      globalPhase = run.phaseNamed("packageobjects")
      val openPackageObjectsTraverser = new Traverser {
        override def traverse(tree: compiler.Tree): Unit = tree match {
          case ModuleDef(_, _, _) =>
            if (tree.symbol.name == nme.PACKAGEkw) {
              openPackageModule(tree.symbol, tree.symbol.owner)
            }
          case ClassDef(_, _, _, _) => () // make it fast
          case _ => super.traverse(tree)
        }
      }
      openPackageObjectsTraverser(unit.body)
      throwIfErrors()

      phase = run.typerPhase
      globalPhase = run.typerPhase
      val typer = newTyper(rootContext(unit))
      typer.context.initRootContext() // need to manually set context mode, otherwise typer.silent will throw exceptions
      unit.body = typer.typed(unit.body).asInstanceOf[compiler.Tree]
      for (workItem <- unit.toCheck) workItem()
      throwIfErrors()
      unit.body
		}

    def scalaToMeta(tree: compiler.Tree) = {
      import scala.meta.internal.ast._
      implicit val c = Scalahost.mkSemanticContext[compiler.type](compiler)
      c.toScalameta(tree, classOf[Source])
    }

    def parseToMeta(code: String) = scalaToMeta(parseAndType(code))
	}

	def loadSource(filename: String): String = scala.io.Source.fromFile(new File(filename)).mkString.trim

  /**
   * TODO should this method take a classpath as parameter ?*/
	def loadCompiler: CompilerProxy = {

    val classPathDefault = Thread.currentThread().getContextClassLoader().getParent.asInstanceOf[URLClassLoader].getURLs.toList.map(_.getFile)
    val classPathFromSBT = Some(System.getProperty("sbt.paths.tqlscalameta.classpath")).filter(_ != null).map(_.split(";")).getOrElse(Array[String]())
    val fullClassPath = classPathDefault ++ classPathFromSBT

    val scalahostPluginJar = fullClassPath.find(_.contains("scalahost")) match {
      case Some(path) => path
      case None => throw new Exception("Path to scalahost jar not found")
    }
    val classPath = fullClassPath.mkString(";")
    val mytb =  scala.reflect.runtime.currentMirror.mkToolBox(options = "-cp " + classPath + " -Xplugin:" + scalahostPluginJar + " -Xplugin-require:scalahost")
    var result: CompilerProxy = null

    /*This is the function which will get called by the compiler api.
    * I hijack it to get only the compiler back*/
    def cont(compilerApi: AnyRef): Unit = {
      val m_compiler = compilerApi.getClass.getDeclaredMethod("compiler")
      val comp = m_compiler.invoke(compilerApi).asInstanceOf[scala.tools.nsc.Global]
      result = new CompilerProxy{
        val tb = mytb
        type Compiler = comp.type
        val compiler: Compiler = comp
      }
      result.init
    }

    val m_withCompilerApi = mytb.getClass.getDeclaredMethod("withCompilerApi")
    val o_withCompilerApi = m_withCompilerApi.invoke(mytb)
    val m_apply = o_withCompilerApi.getClass.getDeclaredMethods.find(_.getName == "apply").get

    try m_apply.invoke(o_withCompilerApi, cont _)
    catch scala.reflect.runtime.ReflectionUtils.unwrapHandler({ case ex => throw ex })

    result
	}

  def load(fileName: String) = loadCompiler.parseToMeta(loadSource(fileName))
}