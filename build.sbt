name := "scalation"

version := "1.0"

scalaVersion := "2.9.3"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0.3"

libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-swing" % _ }

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xexperimental")

sourceDirectory <<= baseDirectory( _ / "src" )

unmanagedSourceDirectories <<= baseDirectory( base => List("src", "examples") map (base / _ ))

