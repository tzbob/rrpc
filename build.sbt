import sbtcrossproject.CrossPlugin.autoImport.crossProject

val http4sVersion = "0.21.0"
ThisBuild / scalaVersion := "2.13.1"

lazy val root = project
  .in(file("."))
  .aggregate(rpc.js, rpc.jvm)
  .settings(
    publish := {},
    publishLocal := {},
  )

lazy val rpc = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(
    scalacOptions ++= Seq(
      "-Ymacro-annotations"
    ),
    name := "rpc",
    version := "0.1-SNAPSHOT",
    libraryDependencies ++= Seq(
      "org.typelevel"     %%% "cats-effect"              % "2.1.1",
      "com.lihaoyi"       %%% "pprint"                   % "0.5.6",
      "io.circe"          %%% "circe-generic"            % "0.12.3",
      "org.scalatest"     %%% "scalatest"                % "3.1.0" % Test,
      "org.scalatestplus" %%% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test,
      "org.scalacheck"    %%% "scalacheck"               % "1.14.1" % Test
    )
  )

lazy val rpcJVM = rpc.jvm
  .settings(
    // Add JVM-specific settings here
    scalaJSProjects := Seq(rpcJS),
    devCommands in scalaJSPipeline += "~reStart",
    pipelineStages in Assets := Seq(scalaJSPipeline),
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-dsl"          % http4sVersion,
      "org.http4s" %% "http4s-blaze-server" % http4sVersion,
      "org.http4s" %% "http4s-circe"        % http4sVersion,
    )
  )
  .enablePlugins(SbtWeb)

lazy val rpcJS = rpc.js
  .settings(
    // Add JS-specific settings here
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      "org.scala-js"                 %%% "scalajs-dom" % "0.9.7",
      "com.softwaremill.sttp.client" %%% "core"        % "2.0.1",
      "com.softwaremill.sttp.client" %%% "circe"       % "2.0.1",
    )
  )
  .enablePlugins(ScalaJSPlugin, ScalaJSWeb)
