name := "complang-crud-api"
version := "0.1"
scalaVersion := "2.13.8"

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-blaze-server" % "0.23.12",
  "org.http4s" %% "http4s-circe" % "0.23.12",
  "org.http4s" %% "http4s-dsl" % "0.23.12",
  "io.circe" %% "circe-generic" % "0.14.1",
  "org.tpolecat" %% "doobie-core" % "1.0.0-RC2",
  "org.tpolecat" %% "doobie-postgres" % "1.0.0-RC2",
  "org.tpolecat" %% "doobie-hikari" % "1.0.0-RC2",
  "org.postgresql" % "postgresql" % "42.3.1",
  "org.typelevel" %% "cats-effect" % "3.3.14"
)
