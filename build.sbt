import sbtcrossproject.{CrossType, crossProject}

name := "Reactive Remote Procedure Call"
val sharedSettings = Seq(
  scalaVersion := "2.12.10",
  scalacOptions += "-Ypartial-unification",
  libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test",
  libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.6"
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
