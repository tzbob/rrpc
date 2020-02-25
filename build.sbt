import sbtcrossproject.{CrossType, crossProject}

val http4sVersion = "0.21.0"

name := "Reactive Remote Procedure Call"
val sharedSettings = Seq(
  scalaVersion := "2.12.10",
  scalacOptions += "-Ypartial-unification",
  libraryDependencies ++= Seq(
    "org.typelevel"     %% "cats-effect"              % "2.1.1",
    "com.lihaoyi"       %% "pprint"                   % "0.5.6",
    "org.scalatest"     %% "scalatest"                % "3.1.0" % Test,
    "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test,
    "org.scalacheck"    %% "scalacheck"               % "1.14.1" % Test
  )
)

lazy val rrpcRoot = project
  .in(file("."))
  .aggregate(sharedJvm, sharedJs, server, client)
  .settings(
    publish := {},
    publishLocal := {},
    publishArtifact := false,
    isSnapshot := true,
    run := {
      (run in server in Compile).evaluated
    }
  )

lazy val server = (project in file("server"))
  .settings(sharedSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-dsl"          % http4sVersion,
      "org.http4s" %% "http4s-blaze-server" % http4sVersion,
    )
  )
  .dependsOn(sharedJvm)

lazy val client = (project in file("client"))
  .settings(sharedSettings)
  .dependsOn(sharedJs)

lazy val shared = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(sharedSettings)

lazy val sharedJvm = shared.jvm
lazy val sharedJs  = shared.js
