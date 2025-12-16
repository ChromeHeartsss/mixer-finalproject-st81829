ThisBuild / scalaVersion := "3.3.4"
ThisBuild / organization := "edu.fp"
ThisBuild / version := "0.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "fp-mixer-analyzer",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "upickle" % "4.1.0"
    )
  )
