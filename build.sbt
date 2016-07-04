
// build.sbt - specifications for Simple Build Tool

name := "scalation"

organization := "scalation"

version := "1.2"

scalaVersion := "2.12.0-M3"

fork := true

// scalacOptions += "-feature"
// scalacOptions += "-unchecked"
scalacOptions += "-deprecation"

scalacOptions += "-Yopt:l:classpath"            // optimize
scalacOptions += "-Yopt-warnings"
scalacOptions += "-Xlint:-adapted-args"         // run lint - disable "adapted-args" (auto tupling used)

javaOptions += "-Xmx2G"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies += "org.jsoup" % "jsoup" % "1.8.2"

libraryDependencies += "junit" % "junit" % "4.11" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.8" % "test->default"

