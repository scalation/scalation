
////////////////////////////////////////////////////////////////////////////////
// build.sbt - specifications for Simple Build Tool

lazy val commonSettings = Seq (

name         := "scalation",
organization := "scalation",
version      := "1.6",
scalaVersion := "2.12.8",
fork         := true,

concurrentRestrictions := Seq(Tags.limitAll(1)),

////////////////////////////////////////////////////////////////////////////////
// Scala options

scalacOptions += "-deprecation",
scalacOptions += "-feature",
scalacOptions += "-Xfatal-warnings",
scalacOptions += "-opt:l:method",               // enable all intra-method optimizations
scalacOptions += "-opt:l:inline",               // enable cross-method optimizations
scalacOptions += "-opt-inline-from:**",         // allow inlining for all classes
scalacOptions += "-opt-warnings",
scalacOptions += "-Xlint:-adapted-args",        // run lint - disable "adapted-args" (auto tupling used)

// scalacOptions += "-feature",
// scalacOptions += "-unchecked",

////////////////////////////////////////////////////////////////////////////////
// Java options

javaOptions += "-Xmx2G"

) // commonSettings

lazy val root = (project in file("."))

  .settings (
    commonSettings,
    name := "scalation_modeling",
    unmanagedBase := baseDirectory.value / "../lib",

////////////////////////////////////////////////////////////////////////////////
// Scala Modules
// @see http://scala-lang.org/documentation/api.html

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,

////////////////////////////////////////////////////////////////////////////////
// Unit Testing

libraryDependencies += "junit" % "junit" % "4.11" % "test",
libraryDependencies += "com.novocode" % "junit-interface" % "0.8" % "test->default"

) // root

////////////////////////////////////////////////////////////////////////////////
// Fast Regex

// libraryDependencies += "dk.brics.automaton" % "automaton" % "1.11-8"

////////////////////////////////////////////////////////////////////////////////
// Java HTML Parser - needed for UCIML

// libraryDependencies += "org.jsoup" % "jsoup" % "1.8.2"

