name := "lmc-scala"

version := "0.1"

//scalaVersion := "2.12.5"
scalaVersion := "0.8.0-RC1"

scalacOptions ++= { if (isDotty.value) Seq("-language:Scala2") else Nil }

val dependencies = Seq(
  Seq(
    "com.novocode" % "junit-interface" % "0.8",
    "com.github.pathikrit" %% "better-files" % "3.4.0",
  ).map(_ % Test),
  Seq(
    "net.liftweb" %% "lift-json" % "3.0.2"
  )
).flatten

libraryDependencies ++= dependencies
   .map(_.withDottyCompat(scalaVersion.value))

enablePlugins(PackPlugin)
packMain := Map("lmc" -> "lmc.Main")

//enablePlugins(ScalaJSPlugin)
//scalaJSUseMainModuleInitializer := true

//enablePlugins(ScalaNativePlugin)

