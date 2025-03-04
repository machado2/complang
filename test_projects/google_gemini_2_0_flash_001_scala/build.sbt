
name := "user-api"
version := "1.0"
scalaVersion := "2.13.8"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "10.2.6",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.2.6",
  "org.postgresql" % "postgresql" % "42.3.1",
  "com.typesafe.slick" %% "slick" % "3.3.3",
  "com.typesafe.slick" %% "slick-hikaricp" % "3.3.3",
  "ch.qos.logback" % "logback-classic" % "1.2.6" % Runtime,
  "com.typesafe.akka" %% "akka-actor-typed" % "2.6.18",
  "com.typesafe.akka" %% "akka-stream" % "2.6.18"
)

scalacOptions += "-Wconf:src=routes/.*:s"

assembly / assemblyJarName := "app.jar"