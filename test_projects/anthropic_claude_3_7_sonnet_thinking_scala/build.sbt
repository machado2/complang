name := "scala-crud-api"
version := "0.1.0"
scalaVersion := "2.13.8"

lazy val akkaVersion = "2.6.19"
lazy val akkaHttpVersion = "10.2.9"
lazy val slickVersion = "3.3.3"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion,
  "com.typesafe.slick" %% "slick" % slickVersion,
  "com.typesafe.slick" %% "slick-hikaricp" % slickVersion,
  "org.postgresql" % "postgresql" % "42.3.6",
  "ch.qos.logback" % "logback-classic" % "1.2.11"
)
