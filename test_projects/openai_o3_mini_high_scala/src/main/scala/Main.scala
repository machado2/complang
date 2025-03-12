package com.complang

import cats.effect.{IO, IOApp, ExitCode}
import org.http4s.{HttpRoutes}
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe._
import doobie._
import doobie.implicits._
import doobie.hikari.HikariTransactor
import scala.concurrent.ExecutionContext.global

object Main extends IOApp {

  // Data models
  case class User(id: Int, name: String, email: String)
  case class CreateUser(name: String, email: String)
  case class CreateUserResponse(id: Int, name: String, email: String)
  case class UpdateUser(name: String, email: String)

  // HTTP routes for CRUD operations on /users
  def createRoutes(xa: Transactor[IO]) = HttpRoutes.of[IO] {

    // POST /users: Create a new user
    case req @ POST -> Root / "users" =>
      for {
        userInput <- req.as[CreateUser]
        id <- sql"INSERT INTO users (name, email) VALUES (${userInput.name}, ${userInput.email}) RETURNING id"
                .query[Int]
                .unique
                .transact(xa)
        response <- Created(CreateUserResponse(id, userInput.name, userInput.email).asJson)
      } yield response

    // GET /users: Get all users
    case GET -> Root / "users" =>
      for {
        users <- sql"SELECT id, name, email FROM users".query[User].to[List].transact(xa)
        response <- Ok(users.asJson)
      } yield response

    // GET /users/{id}: Get a single user by ID
    case GET -> Root / "users" / IntVar(id) =>
      for {
        userOpt <- sql"SELECT id, name, email FROM users WHERE id = $id".query[User].option.transact(xa)
        response <- userOpt match {
          case Some(user) => Ok(user.asJson)
          case None => NotFound()
        }
      } yield response

    // PUT /users/{id}: Update an existing user
    case req @ PUT -> Root / "users" / IntVar(id) =>
      for {
        userUpdate <- req.as[UpdateUser]
        updated <- sql"UPDATE users SET name = ${userUpdate.name}, email = ${userUpdate.email} WHERE id = $id"
                     .update
                     .run
                     .transact(xa)
        response <- if (updated > 0) NoContent() else NotFound()
      } yield response

    // DELETE /users/{id}: Delete a user
    case DELETE -> Root / "users" / IntVar(id) =>
      for {
        deleted <- sql"DELETE FROM users WHERE id = $id".update.run.transact(xa)
        response <- if (deleted > 0) NoContent() else NotFound()
      } yield response
  }

  override def run(args: List[String]): IO[ExitCode] = {
    // Retrieve the PostgreSQL password from the environment variable
    val pgPassword = Option(System.getenv("PGPASSWORD")).getOrElse("")
    // Create a Hikari Transactor for PostgreSQL connection
    val transactorResource = for {
      ce <- IO.executionContext
      xa <- HikariTransactor.newHikariTransactor[IO](
              "org.postgresql.Driver",
              "jdbc:postgresql://host.docker.internal:5432/complang",
              "testuser",
              pgPassword,
              ce
            )
    } yield xa

    // Start the HTTP server on port 8080
    transactorResource.use { xa =>
      val httpApp = createRoutes(xa).orNotFound
      BlazeServerBuilder[IO](global)
        .bindHttp(8080, "0.0.0.0")
        .withHttpApp(httpApp)
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    }
  }
}
