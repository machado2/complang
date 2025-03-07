name := "crud-api"

version := "0.1"

scalaVersion := "2.13.10"

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-ember-server" % "0.23.18",
  "org.http4s" %% "http4s-circe"         % "0.23.18",
  "org.http4s" %% "http4s-dsl"           % "0.23.18",
  "io.circe"   %% "circe-generic"        % "0.14.2",
  "org.tpolecat" %% "doobie-core"         % "1.0.0-RC2",
  "org.tpolecat" %% "doobie-postgres"     % "1.0.0-RC2",
  "org.tpolecat" %% "doobie-hikari"       % "1.0.0-RC2",
  "org.typelevel" %% "cats-effect"         % "3.3.14"
)
