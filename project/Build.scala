import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.2",
    crossScalaVersions := Seq("2.10.2", "2.10.3", "2.10.4", "2.11.0", "2.11.1", "2.11.2", "2.11.3", "2.11.4"),
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    organization := "com.github.begeric",
    libraryDependencies += "org.scalameta" % "scalameta_2.11" % "0.1.0-SNAPSHOT",
    scalacOptions ++= Seq(),
    publishMavenStyle := true,
    publishOnlyWhenOnMaster := publishOnlyWhenOnMasterImpl.value,
    publishTo <<= version { v: String =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    pomIncludeRepository := { x => false },
    publishArtifact in Compile := false,
    publishArtifact in Test := false,
    pomExtra := (
      <url>https://github.com/begueric/tql-scalameta</url>
      <inceptionYear>2014</inceptionYear>
      <licenses>
        <license>
          <name>BSD-like</name>
          <url>http://www.scala-lang.org/downloads/license.html</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git://github.com/begueric/tql-scalameta.git</url>
        <connection>scm:git:git://github.com/begueric/tql-scalameta.git</connection>
      </scm>
      <issueManagement>
        <system>GitHub</system>
        <url>https://github.com/begueric/tql-scalameta/issues</url>
      </issueManagement>
    ),
    publishArtifact in (Compile, packageDoc) := false
  )
  
  lazy val publishOnlyWhenOnMaster = taskKey[Unit]("publish task for Travis (don't publish when building pull requests, only publish when the build is triggered by merge into master)")
  def publishOnlyWhenOnMasterImpl = Def.taskDyn {
    import scala.util.Try
    val travis   = Try(sys.env("TRAVIS")).getOrElse("false") == "true"
    val pr       = Try(sys.env("TRAVIS_PULL_REQUEST")).getOrElse("false") != "false"
    val branch   = Try(sys.env("TRAVIS_BRANCH")).getOrElse("??")
    val snapshot = version.value.trim.endsWith("SNAPSHOT")
    (travis, pr, branch, snapshot) match {
      case (true, false, "master", true) => publish
      case _                             => Def.task ()
    }
  }

  lazy val publishableSettings = Seq(
    publishArtifact in Compile := true,
    publishArtifact in Test := false,
    credentials ++= {
      val mavenSettingsFile = System.getenv("MAVEN_SETTINGS_FILE")
      if (mavenSettingsFile != null) {
        println("Loading Sonatype credentials from " + mavenSettingsFile)
        try {
          import scala.xml._
          val settings = XML.loadFile(mavenSettingsFile)
          def readServerConfig(key: String) = (settings \\ "settings" \\ "servers" \\ "server" \\ key).head.text
          Some(Credentials(
            "Sonatype Nexus Repository Manager",
            "oss.sonatype.org",
            readServerConfig("username"),
            readServerConfig("password")
          ))
        } catch {
          case ex: Exception =>
            println("Failed to load Maven settings from " + mavenSettingsFile + ": " + ex)
            None
        }
      } else {
        for {
          realm <- sys.env.get("TQL_MAVEN_REALM")
          domain <- sys.env.get("TQL_MAVEN_DOMAIN")
          user <- sys.env.get("TQL_MAVEN_USER")
          password <- sys.env.get("TQL_MAVEN_PASSWORD")
        } yield {
          println("Loading Sonatype credentials from environment variables")
          Credentials(realm, domain, user, password)
        }
      }
    }.toList
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
    settings = buildSettings ++ publishableSettings
  ) dependsOn(tqlmacros)

  lazy val tqlscalametamacros: Project = Project(
    "tqlscalametamacros",
    file("tqlscalametamacros"),
    settings = buildSettings ++ macroSettings ++ Seq(libraryDependencies += "org.scalameta" % "scalameta_2.11" % "0.1.0-SNAPSHOT")
  ) dependsOn(tql)

  lazy val tqlscalameta: Project = Project(
    "tqlscalameta",
    file("tqlscalameta"),
    settings = buildSettings ++ publishableSettings ++ Seq(libraryDependencies += "org.scalameta" % "scalameta_2.11" % "0.1.0-SNAPSHOT")
  ) dependsOn(tqlscalametamacros)

}
