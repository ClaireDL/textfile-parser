ThisBuild / scalaVersion     := "2.13.4"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "Template",
    libraryDependencies ++= Seq(
      "com.typesafe" % "config" % "1.4.0",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
    ),
    exportJars := true
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
