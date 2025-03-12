
name := "scala-crud"
version := "1.0"
scalaVersion := "2.12.10"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "10.1.10",
  "com.typesafe.akka" %% "akka-stream" % "2.5.23",
  "com.typesafe.slick" %% "slick" % "3.3.2",
  "org.postgresql" % "postgresql" % "42.2.8",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.10"
)
