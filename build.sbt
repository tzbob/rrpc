import sbtcrossproject.CrossPlugin.autoImport.crossProject

Global / onChangedBuildSource := ReloadOnSourceChanges

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
    addCompilerPlugin(scalafixSemanticdb),
    scalacOptions ++= Seq(
      "-Ymacro-annotations",
      "-Yrangepos",
      "-Ywarn-unused"
    ),
    name := "rpc",
    version := "0.1-SNAPSHOT",
    libraryDependencies ++= Seq(
      "org.typelevel"     %%% "cats-effect"              % "2.1.1",
      "com.lihaoyi"       %%% "pprint"                   % "0.5.6",
      "io.circe"          %%% "circe-core"               % "0.12.3",
      "io.circe"          %%% "circe-parser"             % "0.12.3",
      "io.circe"          %%% "circe-generic"            % "0.12.3",
      "io.7mind.izumi"    %%% "logstage-core"             % "0.10.3-M2",
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
    devCommands in scalaJSPipeline += "~testQuick",
    devCommands in scalaJSPipeline += "~test",
    pipelineStages in Assets := Seq(scalaJSPipeline),
    libraryDependencies ++= Seq(
      "org.http4s"  %% "http4s-dsl"          % http4sVersion,
      "org.http4s"  %% "http4s-blaze-server" % http4sVersion,
      "org.http4s"  %% "http4s-circe"        % http4sVersion,
      "com.lihaoyi" %% "os-lib"              % "0.6.2"
    )
  )
  .enablePlugins(WebScalaJSBundlerPlugin)

lazy val rpcJS = rpc.js
  .settings(
    // Add JS-specific settings here
    scalaJSUseMainModuleInitializer := true,
    scalaJSModuleKind := ModuleKind.CommonJSModule,
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom"       % "1.0.0",
      "be.tzbob"     %%% "scala-js-snabbdom" % "0.5.0",
    )
  )
  .enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)
