lazy val commonSettings = Seq(
  organization := "br.bireme",
  version := "0.1.0",
  scalaVersion := "2.12.2"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "BVS-InfoButton"
  )

val akkaVersion = "10.0.7" //"10.0.6"
val playJsonVersion = "2.6.0-RC2" // 2.6.0-M7"
val scalaXmlVersion = "1.0.6"
val dom4jVersion = "2.0.0"
val scalaLoggingVersion = "3.7.1"
val logbackVersion = "1.2.3"
val scalaTestVersion = "3.0.3"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % akkaVersion,
  "com.typesafe.play" %% "play-json" % playJsonVersion,
  "org.scala-lang.modules" % "scala-xml_2.12" % scalaXmlVersion,
  "org.dom4j" % "dom4j" % dom4jVersion,
  "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
  "ch.qos.logback" % "logback-classic" % logbackVersion,
  "org.scalactic" %% "scalactic" % scalaTestVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)

logBuffered in Test := false
trapExit :=  false  // To allow System.exit() without an exception (TestIndex.scala)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ywarn-unused")
