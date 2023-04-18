lazy val commonSettings = Seq(
  organization := "org.bireme",
  version := "1.0.0",
  scalaVersion := "2.13.10" //"2.12.8"  // org.scala-lang.modules:scala-xml _2.13, _2.12
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "BVS-InfoButton"
  )

val akkaVersion = "10.5.0" //"10.1.12"
val playJsonVersion = "2.9.4" //"2.9.0"
val scalaXmlVersion = "2.1.0" //"1.3.0"
val dom4jVersion = "2.1.4" //"2.1.3"
val scalaLoggingVersion = "3.9.5" //"3.9.2"
val logbackVersion = "1.4.6" //"1.2.3"
val scalaTestVersion = "3.2.15" //"3.2.0"
val luceneVersion = "9.5.0" //"8.6.0"
val scalajHttpVersion = "2.4.2" //"2.4.1"

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
  "org.scalaj" %% "scalaj-http" % scalajHttpVersion
)

Test / logBuffered := false
trapExit := false  // To allow System.exit() without an exception (TestIndex.scala)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ywarn-unused")

assembly / assemblyMergeStrategy := {
  case "module-info.class" => MergeStrategy.first //MergeStrategy.discard
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}

assembly / test := {}

