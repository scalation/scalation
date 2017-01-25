
////////////////////////////////////////////////////////////////////////////////
// build.sbt - specifications for Simple Build Tool

name := "scalation"

organization := "scalation"

version := "1.2"

scalaVersion := "2.12.1"

fork := true

////////////////////////////////////////////////////////////////////////////////
// Scala options

scalacOptions += "-deprecation"
scalacOptions += "-feature"
scalacOptions += "-Xfatal-warnings"
scalacOptions += "-opt:l:classpath"             // optimize
scalacOptions += "-opt-warnings"
scalacOptions += "-Xlint:-adapted-args"         // run lint - disable "adapted-args" (auto tupling used)
// scalacOptions += "-feature"
// scalacOptions += "-unchecked"

////////////////////////////////////////////////////////////////////////////////
// Java options

javaOptions += "-Xmx2G"

////////////////////////////////////////////////////////////////////////////////
// Scala Modules
// @see http://scala-lang.org/documentation/api.html

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

////////////////////////////////////////////////////////////////////////////////
// Unit Testing

libraryDependencies += "junit" % "junit" % "4.11" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.8" % "test->default"

////////////////////////////////////////////////////////////////////////////////
// Fast Regex

libraryDependencies += "dk.brics.automaton" % "automaton" % "1.11-8"

////////////////////////////////////////////////////////////////////////////////
// Java HTML Parser - needed for UCIML

// libraryDependencies += "org.jsoup" % "jsoup" % "1.8.2"

