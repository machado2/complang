name := "scala-crud"
version := "1.0"
scalaVersion := "2.13.8"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "10.2.6",
  "com.typesafe.akka" %% "akka-stream" % "2.6.14",
  "com.typesafe.slick" %% "slick" % "3.3.3",
  "org.postgresql" % "postgresql" % "42.2.24",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.2.6",
  "ch.qos.logback" % "logback-classic" % "1.2.6" % Runtime,
  "com.h2database" % "h2" % "2.1.214" % "test" // H2 database for testing
)

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-Xlint",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard"
)