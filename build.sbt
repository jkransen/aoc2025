ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.4"

lazy val root = (project in file("."))
  .settings(
    name := "aoc2025",
    idePackagePrefix := Some("nl.kransen.aoc2025")
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test
