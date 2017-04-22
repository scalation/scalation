
////////////////////////////////////////////////////////////////////////////////
// build.sbt - specifications for Simple Build Tool

lazy val commonSettings = Seq (

name         := "scalation",
organization := "scalation",
version      := "1.3",
scalaVersion := "2.12.2",
fork         := true,

////////////////////////////////////////////////////////////////////////////////
// Scala options

scalacOptions += "-deprecation",
scalacOptions += "-feature",
scalacOptions += "-Xfatal-warnings",
scalacOptions += "-opt:l:classpath",            // optimize
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
//  unmanagedBase := baseDirectory.value / "../scalation_mathstat/target/scala-2.12/scalation_mathstat_2.12-1.3.jar",

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
//).dependsOn (mathstat) // root

////////////////////////////////////////////////////////////////////////////////
// Fast Regex

// libraryDependencies += "dk.brics.automaton" % "automaton" % "1.11-8"

////////////////////////////////////////////////////////////////////////////////
// Java HTML Parser - needed for UCIML

// libraryDependencies += "org.jsoup" % "jsoup" % "1.8.2"

