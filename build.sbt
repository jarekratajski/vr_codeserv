name := "codevide-serv"

version := "0.1"

scalaVersion := "2.12.3"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "10.0.10",
  "ch.megard" %% "akka-http-cors" % "0.2.1",
  "com.typesafe.play" %% "play-json" % "2.6.0",
  "com.github.javaparser" % "javaparser-core" % "3.3.4",
  "org.scala-lang.modules" % "scala-xml_2.12" % "1.0.6",
  "com.typesafe.akka" %% "akka-http-testkit" % "10.0.10" % Test
)