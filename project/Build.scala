import sbt._
import Keys._


object UnfilteredUtils extends Build {
  val description = SettingKey[String]("description")

  resolvers ++= repos

  val parentSettings = Defaults.defaultSettings ++ Seq(
    parallelExecution := false,
    organization := "com.recursivity.web",
    version := "0.0.1-SNAPSHOT",
    crossScalaVersions := Seq("2.10.1"),
    scalaVersion <<= (crossScalaVersions) {
      versions => versions.head
    },
    packageOptions <<= (packageOptions, name, version, organization) map {
      (opts, title, version, vendor) =>
        opts :+ Package.ManifestAttributes(
          "Created-By" -> "Simple Build Tool",
          "Built-By" -> System.getProperty("user.name"),
          "Build-Jdk" -> System.getProperty("java.version"),
          "Specification-Title" -> title,
          "Specification-Version" -> version,
          "Specification-Vendor" -> vendor,
          "Implementation-Title" -> title,
          "Implementation-Version" -> version,
          "Implementation-Vendor-Id" -> vendor,
          "Implementation-Vendor" -> vendor
        )
    }
  )



  val repos = Seq("Sonatype Nexus releases" at "https://oss.sonatype.org/content/repositories/releases",
    "Sonatype Nexus snapshots" at "https://oss.sonatype.org/content/repositories/snapshots", "Twitter" at "http://maven.twttr.com/")


  val sonatypeSnapshots = "Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  object Dependencies {
    val uv = "0.6.8"
    val specs2 = "org.specs2" %% "specs2" % "1.14" % "test"
    val scalate = "org.fusesource.scalate" %% "scalate-core" % "1.6.1"
    val scalaCompiler = "org.scala-lang" % "scala-compiler" % "2.10.1"
    val scalaLang = "org.scala-lang" % "scala-library" % "2.10.1"
    val jodaTime = "joda-time" % "joda-time" % "1.6.1"
    val unfilteredFilter = "net.databinder" %% "unfiltered-filter" % uv
    val unfilteredJetty = "net.databinder" %% "unfiltered-jetty" % uv % "provided" 
   // val unfiltered = "net.databinder" %% "unfiltered-netty" % uv
    val liftJson = "net.liftweb" %% "lift-json" % "2.5-RC5"
    val scalaz = "org.scalaz" %% "scalaz-core" % "7.0.0"
  }

  import Dependencies._

  lazy val sitegen = Project("unfiltered-utils", file("."),
    settings = parentSettings ++ seq(sbtassembly.Plugin.assemblySettings: _*))
    .settings(libraryDependencies := Seq(scalate,scalaCompiler,scalaLang,unfilteredFilter,unfilteredJetty,liftJson,scalaz,jodaTime, specs2),
    publishArtifact in Compile := false,
    description := "Parent project",
	resolvers ++= repos)
}
