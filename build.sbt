lazy val commonSettings = Seq(
  organization := "org.bireme",
  version := "1.0.0",
  scalaVersion := "3.3.7" //"2.13.16" //"2.12.8"  // org.scala-lang.modules:scala-xml _2.13, _2.12
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "BVS-InfoButton"
  )

val akkaVersion = "10.5.3" //"10.5.0"
val playJsonVersion = "2.10.8" //"2.9.4" //"2.9.0"
val scalaXmlVersion = "2.4.0" //"2.1.0" //"1.3.0"
val dom4jVersion = "2.2.0" //"2.1.3"
val scalaLoggingVersion = "3.9.6" //"3.9.2"
val logbackVersion = "1.5.23" //"1.4.12" //"1.4.6"
val scalaTestVersion = "3.2.19" //"3.2.15"
val luceneVersion = "10.3.2" //"9.6.0" //"9.5.0"
val jakartaServletApiVersion = "6.1.0" //"6.0.0"
val log4jVersion = "2.25.3" //"2.19.0"
val sttpVersion = "3.11.0"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % akkaVersion,
  "com.typesafe.play" %% "play-json" % playJsonVersion,
  "org.scala-lang.modules" %% "scala-xml" % scalaXmlVersion,
  "org.dom4j" % "dom4j" % dom4jVersion,
  "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
  "ch.qos.logback" % "logback-classic" % logbackVersion,
  "ch.qos.logback" % "logback-core" % logbackVersion,
  "org.scalactic" %% "scalactic" % scalaTestVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "org.apache.lucene" % "lucene-core" % luceneVersion,
  "org.apache.lucene" % "lucene-analysis-common" % luceneVersion,
  //"org.apache.lucene" % "lucene-analyzers-common" % luceneVersion,
  "org.apache.lucene" % "lucene-queryparser" % luceneVersion,
  "org.apache.lucene" % "lucene-queries" % luceneVersion,
  "org.apache.lucene" % "lucene-backward-codecs" % luceneVersion,
  "com.softwaremill.sttp.client3" %% "core" % sttpVersion,
  "jakarta.servlet" % "jakarta.servlet-api" % jakartaServletApiVersion % "provided",
  "org.apache.logging.log4j" % "log4j-api" % log4jVersion,
)

Test / logBuffered := false
trapExit := false  // To allow System.exit() without an exception (TestIndex.scala)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

enablePlugins(SbtWar)
//enablePlugins(JettyPlugin)
//enablePlugins(TomcatPlugin)

assembly / assemblyMergeStrategy := {
  case "module-info.class" => MergeStrategy.first //MergeStrategy.discard
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}

assembly / test := {}
