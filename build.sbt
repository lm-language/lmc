name := "lmc-scala"

version := "0.1"

scalaVersion := "2.12.5"
//scalaVersion := "0.8.0-RC1"

val circeVersion = "0.9.3"

scalacOptions ++= { if (isDotty.value) Seq("-language:Scala2") else Nil }

val dependencies = Seq(
  Seq(
    "com.novocode" % "junit-interface" % "0.8" % "test",
    "org.scalatest" %% "scalatest" % "3.0.5" % "test",
    "com.github.pathikrit" %% "better-files" % "3.4.0",
  ),
  Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
  ).map(_ % circeVersion)
).flatten

libraryDependencies ++= dependencies
//   .map(_.withDottyCompat(scalaVersion.value))

