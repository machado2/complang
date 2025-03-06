name := "ComplangCRUD"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.12.15"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http"   % "10.2.6",
  "com.typesafe.akka" %% "akka-stream" % "2.6.17",
  "com.typesafe.akka" %% "akka-actor" % "2.6.17",
  "com.typesafe.akka" %% "akka-slf4j" % "2.6.17",
  "ch.qos.logback"    %  "logback-classic" % "1.2.6",
  "com.typesafe.slick" %% "slick" % "3.3.3",
  "org.postgresql" % "postgresql" % "42.2.24",
  "com.typesafe.slick" %% "slick-hikaricp" % "3.3.3", // HikariCP
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.2.6"
)

mainClass in Assembly := Some("Main")