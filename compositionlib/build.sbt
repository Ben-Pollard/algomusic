name := "scala"

version := "0.1"

scalaVersion := "2.12.4"

fork := true

libraryDependencies += "com.googlecode.soundlibs" % "tritonus-all" % "0.3.7.2"

resolvers ++= Seq(
  "scala-tools" at "https://oss.sonatype.org/content/groups/scala-tools",
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Second Typesafe repo" at "http://repo.typesafe.com/typesafe/maven-releases/",
  "Mesosphere Public Repository" at "http://downloads.mesosphere.io/maven",
  Resolver.sonatypeRepo("public"),
  Resolver.url("bintray-sbt-plugins", url("http://dl.bintray.com/sbt/sbt-plugin-releases"))
)