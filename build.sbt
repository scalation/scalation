name := "scalation"

version := "1.0"

organization := "scalation"

scalaVersion := "2.10.2"

crossScalaVersions := Seq("2.9.2", "2.9.3", "2.10.2")

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"

libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-swing" % _ }

libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-library" % _ }

libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-compiler" % _ }

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xexperimental")

sourceDirectory in Compile <<= baseDirectory( _ / "." )

unmanagedSourceDirectories in Compile <<= baseDirectory(base => Seq("src", "examples") map (base / _))

