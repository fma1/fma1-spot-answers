import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "fma1-spot-answers",
    libraryDependencies += scalaTest % Test,
    fork := true,
    cancelable in Global := true
  )
