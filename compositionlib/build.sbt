//name := "scala"
//version := "0.1"
//scalaVersion := " 2.13.5"
//fork := true
//libraryDependencies += "com.googlecode.soundlibs" % "tritonus-all" % "0.3.7.2"

ThisBuild / scalaVersion := "2.12.7"
ThisBuild / fork := true

lazy val hello = (project in file("."))
  .settings(
    name := "Hello",
    fork := true,
    libraryDependencies ++= Seq(
//      "com.googlecode.soundlibs" % "tritonus-all" % "0.3.7.2",
      "org.hipparchus" % "hipparchus-optim" % "1.8",
      "com.github.psambit9791" % "jdsp" % "2.0.0",
      "org.plotly-scala" %% "plotly-core" % "0.8.3",
      "org.plotly-scala" %% "plotly-render" % "0.8.3",
      "org.scalanlp" %% "breeze" % "2.1.0",
      "org.tensorflow" % "tensorflow-core-platform-gpu" % "0.4.2",
      "org.tensorflow" % "tensorflow-framework" % "0.4.2"
    )
  )
resolvers ++= Seq(Resolver.sonatypeRepo("public"))

//resolvers ++= Seq(
//  "scala-tools" at "https://oss.sonatype.org/content/groups/scala-tools",
//  "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/",
//  "Second Typesafe repo" at "https://repo.typesafe.com/typesafe/maven-releases/",
//  "Mesosphere Public Repository" at "https://downloads.mesosphere.io/maven",
//  Resolver.sonatypeRepo("public"),
//  Resolver.sonatypeRepo("releases"),
//  Resolver.url("bintray-sbt-plugins", url("https://dl.bintray.com/sbt/sbt-plugin-releases"))
//)
