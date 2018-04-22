name := "lmc-scala"

version := "0.1"

scalaVersion := "2.12.5"

libraryDependencies += "com.novocode" % "junit-interface" % "0.8" % "test->default"

val circeVersion = "0.9.3"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.4.0"

