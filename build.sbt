organization := "com.github.biopet"
name := "biopet-config-utils"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.11"

resolvers += Resolver.mavenLocal

libraryDependencies += "com.github.biopet" %% "biopet-common-utils" % "0.1.0-SNAPSHOT"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.3"
libraryDependencies += "com.gilt" % "jerkson_2.11" % "0.6.9"
libraryDependencies += "org.yaml" % "snakeyaml" % "1.17"
libraryDependencies += "org.scalaj" % "scalaj-collection_2.11" % "1.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % Test
libraryDependencies += "org.testng" % "testng" % "6.8" % Test
