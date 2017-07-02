import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys.previousArtifacts

organization := "org.reactivemongo"

name := "reactivemongo-play-json"

scalaVersion in ThisBuild := "2.11.11"

version ~= { ver =>
  sys.env.get("RELEASE_SUFFIX") match {
    case Some(suffix) => s"${ver}-${suffix}"
    case _ => ver
  }
}

crossScalaVersions in ThisBuild := Seq("2.11.11", "2.12.2")

crossVersion in ThisBuild := CrossVersion.binary

scalacOptions ++= Seq(
  "-encoding", "UTF-8", "-target:jvm-1.8",
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings",
  "-Xlint",
  "-Ywarn-numeric-widen",
  "-Ywarn-dead-code",
  "-Ywarn-value-discard",
  "-Ywarn-infer-any",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-g:vars"
)

scalacOptions in Compile ++= {
  if (!scalaVersion.value.startsWith("2.11.")) Nil
  else Seq(
    "-Yconst-opt",
    "-Yclosure-elim",
    "-Ydead-code",
    "-Yopt:_"
  )
}

scalacOptions in Test ~= {
  _.filterNot(_ == "-Xfatal-warnings")
}

scalacOptions in (Compile, console) ~= {
  _.filterNot { opt => opt.startsWith("-X") || opt.startsWith("-Y") }
}

scalacOptions in (Test, console) ~= {
  _.filterNot { opt => opt.startsWith("-X") || opt.startsWith("-Y") }
}

scalacOptions in (Compile, doc) ++= Seq(
  "-Ywarn-dead-code", "-Ywarn-unused-import", "-unchecked", "-deprecation",
  /*"-diagrams", */"-implicits", "-skip-packages", "samples") ++
  Opts.doc.title("ReactiveMongo Play JSON API") ++
  Opts.doc.version(Release.major.value)

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/")

val playLower = "2.5.0"
val playUpper = "2.6.0-M1"

val playVer = Def.setting[String] {
  sys.env.get("PLAY_VERSION").getOrElse {
    if (scalaVersion.value startsWith "2.11.") playLower
    else playUpper
  }
}

libraryDependencies ++= Seq(
  "org.reactivemongo" %% "reactivemongo" % (version in ThisBuild).value % "provided" cross CrossVersion.binary,
  "com.typesafe.play" %% "play-json" % playVer.value % "provided" cross CrossVersion.binary)

// Test
fork in Test := false

testOptions in Test += Tests.Cleanup(cl => {
  import scala.language.reflectiveCalls
  val c = cl.loadClass("Common$")
  type M = { def close(): Unit }
  val m: M = c.getField("MODULE$").get(null).asInstanceOf[M]
  m.close()
})

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.8.6",
  "org.slf4j" % "slf4j-simple" % "1.7.13").map(_ % Test)

// Travis CI
val travisEnv = taskKey[Unit]("Print Travis CI env")

travisEnv in Test := { // test:travisEnv from SBT CLI
  val specs = List[(String, List[String])](
    "PLAY_VERSION" -> List(playLower, playUpper)
  )

  lazy val integrationEnv = specs.flatMap {
    case (key, values) => values.map(key -> _)
  }.combinations(specs.size).toList

  @inline def integrationVars(flags: List[(String, String)]): String =
    flags.map { case (k, v) => s"$k=$v" }.mkString(" ")

  def integrationMatrix =
    integrationEnv.map(integrationVars).map { c => s"  - $c" }

  def matrix = (("env:" +: integrationMatrix :+
    "matrix: " :+ "  exclude: ") ++ (
    integrationEnv.flatMap { flags =>
      if (/* time-compat exclusions: */
        flags.contains("PLAY_VERSION" -> playUpper)) {
        List(
          "    - scala: 2.11.8",
          s"      env: ${integrationVars(flags)}"
        )
      } else if (/* time-compat exclusions: */
        flags.contains("PLAY_VERSION" -> playLower)) {
        List(
          "    - scala: 2.12.1",
          s"      env: ${integrationVars(flags)}"
        )
      } else List.empty[String]
    })
  ).mkString("\r\n")

  println(s"# Travis CI env\r\n$matrix")
}

// Publish
val previousVersion = "0.12.0"
val mimaSettings = mimaDefaultSettings ++ Seq(
  previousArtifacts := Set(
    organization.value %% moduleName.value % previousVersion)
)

lazy val publishSettings = {
  @inline def env(n: String): String = sys.env.get(n).getOrElse(n)

  val repoName = env("PUBLISH_REPO_NAME")
  val repoUrl = env("PUBLISH_REPO_URL")

  mimaSettings ++ Seq(
    publishMavenStyle := true,
    publishArtifact in Test := false,
    publishTo := Some(repoUrl).map(repoName at _),
    credentials += Credentials(repoName, env("PUBLISH_REPO_ID"),
        env("PUBLISH_USER"), env("PUBLISH_PASS")),
    pomIncludeRepository := { _ => false },
    licenses := {
      Seq("Apache 2.0" ->
        url("http://www.apache.org/licenses/LICENSE-2.0"))
    },
    homepage := Some(url("http://reactivemongo.org")),
    autoAPIMappings := true,
    pomExtra := (
      <scm>
        <url>git://github.com/ReactiveMongo/ReactiveMongo-Play-Json.git</url>
        <connection>scm:git://github.com/ReactiveMongo/ReactiveMongo-Play-Json.git</connection>
      </scm>
      <developers>
        <developer>
          <id>sgodbillon</id>
          <name>Stephane Godbillon</name>
          <url>http://stephane.godbillon.com</url>
        </developer>
      </developers>))
}

// FindBugs
import de.johoop.findbugs4sbt.{ FindBugs, ReportType }, FindBugs.{
  findbugsExcludeFilters, findbugsReportPath, findbugsReportType,
  findbugsSettings
}

findbugsSettings

findbugsExcludeFilters := Some(
  scala.xml.XML.loadFile(baseDirectory.value / "project" / (
    "findbugs-exclude-filters.xml"))
)

findbugsReportType := Some(ReportType.PlainHtml)

findbugsReportPath := Some(target.value / "findbugs.html")

// Scalariform
import scalariform.formatter.preferences._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys

scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value.
  setPreference(AlignParameters, false).
  setPreference(AlignSingleLineCaseStatements, true).
  setPreference(CompactControlReadability, false).
  setPreference(CompactStringConcatenation, false).
  setPreference(DoubleIndentClassDeclaration, true).
  setPreference(FormatXml, true).
  setPreference(IndentLocalDefs, false).
  setPreference(IndentPackageBlocks, true).
  setPreference(IndentSpaces, 2).
  setPreference(MultilineScaladocCommentsStartOnFirstLine, false).
  setPreference(PreserveSpaceBeforeArguments, false).
  setPreference(PreserveDanglingCloseParenthesis, true).
  setPreference(RewriteArrowSymbols, false).
  setPreference(SpaceBeforeColon, false).
  setPreference(SpaceInsideBrackets, false).
  setPreference(SpacesAroundMultiImports, true).
  setPreference(SpacesWithinPatternBinders, true)

scapegoatVersion := "1.3.0"

scapegoatReports := Seq("xml")

lazy val root = (project in file(".")).
  settings(publishSettings ++ Release.settings)
