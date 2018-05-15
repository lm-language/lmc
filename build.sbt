name := "lmc-scala"

version := "0.1"

val useDotty = true

scalaVersion := { if (useDotty) "0.8.0-RC1" else "2.12.5" }

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

libraryDependencies ++= {
  if (useDotty)
    dependencies
      .map(_.withDottyCompat(scalaVersion.value))
  else
    dependencies
}

enablePlugins(PackPlugin)
packMain := Map("lmc" -> "lmc.Main")

// Settings for JS build
// enablePlugins(ScalaJSPlugin)
// scalaJSUseMainModuleInitializer := true

