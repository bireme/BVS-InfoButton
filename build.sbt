lazy val commonSettings = Seq(
  organization := "org.bireme",
  version := "1.0.0",
  scalaVersion := "2.12.8"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "BVS-InfoButton"
  )

val akkaVersion = "10.1.7" //"10.1.5"
val playJsonVersion = "2.7.2" //"2.6.10"
val scalaXmlVersion = "1.1.1" // "1.0.6"
val dom4jVersion = "2.1.1" // "2.1.0"
val scalaLoggingVersion = "3.9.2" //"3.9.0"
val logbackVersion = "1.2.3"
val scalaTestVersion = "3.0.7" // "3.0.5"
val hairyfotrVersion = "0.1.17"
val luceneVersion = "8.0.0" // "7.5.0"
val httpComponentsVersion = "4.5.7" //"4.5.6"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % akkaVersion,
  "com.typesafe.play" %% "play-json" % playJsonVersion,
  "org.scala-lang.modules" % "scala-xml_2.12" % scalaXmlVersion,
  "org.dom4j" % "dom4j" % dom4jVersion,
  "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
  "ch.qos.logback" % "logback-classic" % logbackVersion,
  "ch.qos.logback" % "logback-core" % logbackVersion,
  "org.scalactic" %% "scalactic" % scalaTestVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "org.apache.lucene" % "lucene-core" % luceneVersion,
  "org.apache.lucene" % "lucene-analyzers-common" % luceneVersion,
  "org.apache.lucene" % "lucene-queryparser" % luceneVersion,
  "org.apache.lucene" % "lucene-queries" % luceneVersion,
  "org.apache.lucene" % "lucene-backward-codecs" % luceneVersion,
  "org.apache.httpcomponents" % "httpclient" % httpComponentsVersion
)

logBuffered in Test := false
trapExit := false  // To allow System.exit() without an exception (TestIndex.scala)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ywarn-unused")
addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % hairyfotrVersion)
