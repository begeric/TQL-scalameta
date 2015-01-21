import sbt._
import Keys._

object BuildSettings {

  val buildSettings = Defaults.defaultSettings ++ Seq(
    version := "0.1-SNAPSHOT",
    scalacOptions ++= Seq("-optimize", "-feature", /*"-Yinline-warnings",*/ "-deprecation"),
    //javaOptions := Seq("-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5140"),
    scalaVersion := "2.11.5",
    crossScalaVersions := Seq("2.10.2", "2.10.3", "2.10.4", "2.11.0", "2.11.1", "2.11.2", "2.11.3", "2.11.4", "2.11.5"),
    resolvers += "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases",
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    libraryDependencies += "org.scalameta" % "scalameta_2.11" % "0.1.0-M0"
  ) ++ PublishSettings.publishSettings
  
  
  val publishableSettings = PublishSettings.publishableSettings
  val macroSettings = MacroSettings.macroSettings

  def exposeClasspaths(projectName: String) = Seq(
     fullClasspath in Compile := {
      val defaultValue = (fullClasspath in Compile).value
      val classpath = defaultValue.files.map(_.getAbsolutePath)
      System.setProperty("sbt.paths." + projectName + ".classpath", classpath.mkString(java.io.File.pathSeparatorChar.toString))
      defaultValue
    }
  )
}