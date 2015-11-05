
// build.sbt - specifications for Simple Build Tool

name := "scalation"

organization := "scalation"

version := "1.2"

scalaVersion := "2.12.0-M2"

crossScalaVersions := Seq("2.12.0-M2", "2.11.7")

fork := true

// scalacOptions += "-feature"

// javaOptions += "-Xmx4G

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies += "org.jsoup" % "jsoup" % "1.8.2"

