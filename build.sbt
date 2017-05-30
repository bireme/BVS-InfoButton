lazy val commonSettings = Seq(
  organization := "br.bireme",
  version := "0.1.0",
  scalaVersion := "2.12.2"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "InfoButtonPOC"
  )

val akkaVersion = "10.0.6"
val playJsonVersion = "2.6.0-M7"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % akkaVersion,
  "com.typesafe.play" %% "play-json" % playJsonVersion
)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
