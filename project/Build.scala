import sbt._
import Keys._

object TQLBuild extends Build {
  import BuildSettings._

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings ++ Seq(
      run <<= run in Compile in tqlscalameta)
  ) aggregate(tqlmacros, tql, tqlscalametamacros, tqlscalameta)

  lazy val tqlmacros: Project = Project(
    "tqlmacros",
    file("tqlmacros"),
    settings = buildSettings ++ publishableSettings ++ macroSettings
  )

  lazy val tql: Project = Project(
    "tql",
    file("tql"),
    settings = buildSettings ++ publishableSettings
  ) dependsOn(tqlmacros)

  lazy val tqlscalametamacros: Project = Project(
    "tqlscalametamacros",
    file("tqlscalametamacros"),
    settings = buildSettings ++ publishableSettings ++
                macroSettings ++ Seq(libraryDependencies += "org.scalameta" % "scalameta_2.11" % metaVersion)
  ) dependsOn(tql)

  lazy val scalaMeterFramework = new TestFramework("org.scalameter.ScalaMeterFramework")

  lazy val tqlscalameta: Project = Project(
    "tqlscalameta",
    file("tqlscalameta"),
    settings = buildSettings ++ publishableSettings ++ Seq(
        libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
        libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.3" % "test",
        libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.5" % "test",
        libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.7-SNAPSHOT" % "test",
        libraryDependencies += "org.scalameta" % "scalahost" % metaVersion cross CrossVersion.full,
        testFrameworks += scalaMeterFramework,
        testOptions += Tests.Argument(scalaMeterFramework, "-silent"),
        //fork in Test := true,
        parallelExecution in Test := false,
        logBuffered := false,
        initialCommands in console := """
          import tools.ScalaToTree._
          import scala.meta.tql.ScalaMetaTraverser._
          import scala.meta.syntactic.ast._
          import scala.meta.syntactic._
          import scala.meta.dialects.Scala211
          """
        ) ++ exposeClasspaths("tqlscalameta")
  ) dependsOn(tqlscalametamacros)

}
