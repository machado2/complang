name := "complang"

version := "0.1"

scalaVersion := "2.13.8"

val doobieVersion = "0.13.4"
val circeVersion = "0.14.1"

libraryDependencies ++= Seq(
  "org.tpolecat" %% "doobie-core"     % doobieVersion,
  "org.tpolecat" %% "doobie-postgres" % doobieVersion,
  "org.tpolecat" %% "doobie-hikari"   % doobieVersion,
  "io.circe"      %% "circe-core"     % circeVersion,
  "io.circe"      %% "circe-generic"  % circeVersion,
  "io.circe"      %% "circe-parser"   % circeVersion
)

containerPort in Docker := Some(8080)
dockerExposedPorts := Seq(8080)
