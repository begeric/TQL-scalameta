import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    version := "1.0.0",
    scalaVersion := "2.11.2",
    crossScalaVersions := Seq("2.10.2", "2.10.3", "2.10.4", "2.11.0", "2.11.1", "2.11.2", "2.11.3", "2.11.4"),
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    libraryDependencies += "org.scalameta" % "scalameta_2.11" % "0.1.0-SNAPSHOT",
    scalacOptions ++= Seq()
  )

  val macroSettings = Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies := {
        CrossVersion.partialVersion(scalaVersion.value) match {
          // if Scala 2.11+ is used, quasiquotes are available in the standard distribution
          case Some((2, scalaMajor)) if scalaMajor >= 11 =>
            libraryDependencies.value
          // in Scala 2.10, quasiquotes are provided by macro paradise
          case Some((2, 10)) =>
            libraryDependencies.value ++ Seq(
              compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full),
              "org.scalamacros" %% "quasiquotes" % "2.0.1" cross CrossVersion.binary)
        }
      }
    )
}

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
    settings = buildSettings ++ macroSettings
  )

  lazy val tql: Project = Project(
    "tql",
    file("tql"),
    settings = buildSettings
  ) dependsOn(tqlmacros)

  lazy val tqlscalametamacros: Project = Project(
    "tqlscalametamacros",
    file("tqlscalametamacros"),
    settings = buildSettings ++ macroSettings
  ) dependsOn(tql)

  lazy val tqlscalameta: Project = Project(
    "tqlscalameta",
    file("tqlscalameta"),
    settings = buildSettings
  ) dependsOn(tqlscalametamacros)

}
