ThisBuild / scalaVersion := "3.3.4"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "paperfold"

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := "paperfold-core",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.0.2" % Test
  )

lazy val coreJS = core.js
lazy val coreJVM = core.jvm

lazy val game = project
  .in(file("game"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(coreJS)
  .settings(
    name := "paperfold-game",
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0",
    Compile / fastLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value / ".." / "web" / "js",
    Compile / fullLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value / ".." / "web" / "js"
  )

lazy val root = project
  .in(file("."))
  .aggregate(coreJS, coreJVM, game)
  .settings(
    name := "paperfold",
    publish / skip := true
  )
