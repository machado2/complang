package example

import cats.effect.{IO, IOApp, Resource}
import cats.implicits._
import doobie._
import doobie.hikari.HikariTransactor
import doobie.implicits._
import doobie.util.ExecutionContexts
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.circe._
import io.circe.generic.auto._
import org.http4s.implicits._
import org.http4s.ember.server.EmberServerBuilder

object Main extends IOApp.Simple {

  // Data models
  case class User(id: Option[Int], name: String, email: String)
  case class UserCreate(name: String, email: String)

  // Create a transactor resource that connects to PostgreSQL.
  // It reads the password from the PGPASSWORD environment variable.
  def createTransactor: Resource[IO, HikariTransactor[IO]] = {
    for {
      connectEC <- ExecutionContexts.fixedThreadPool[IO](32)
      pgPassword = sys.env.getOrElse("PGPASSWORD", "defaultPassword")
      xa <- HikariTransactor.newHikariTransactor[IO](
              "org.postgresql.Driver",
              "jdbc:postgresql://host.docker.internal:5432/complang",
              "testuser",
              pgPassword,
              connectEC
      )
    } yield xa
  }

  // Database operations
  def createUser(user: UserCreate, xa: Transactor[IO]): IO[User] =
    sql"INSERT INTO users (name, email) VALUES (${user.name}, ${user.email})"
      .update
      .withUniqueGeneratedKeys[Int]("id")
      .map(id => User(Some(id), user.name, user.email))
      .transact(xa)

  def getUsers(xa: Transactor[IO]): IO[List[User]] =
    sql"SELECT id, name, email FROM users".query[User].to[List].transact(xa)

  def getUser(id: Int, xa: Transactor[IO]): IO[Option[User]] =
    sql"SELECT id, name, email FROM users WHERE id = $id".query[User].option.transact(xa)

  def updateUser(id: Int, user: UserCreate, xa: Transactor[IO]): IO[Int] =
    sql"UPDATE users SET name = ${user.name}, email = ${user.email} WHERE id = $id".update.run.transact(xa)

  def deleteUser(id: Int, xa: Transactor[IO]): IO[Int] =
    sql"DELETE FROM users WHERE id = $id".update.run.transact(xa)

  // HTTP Routes definition for CRUD operations
  def userRoutes(xa: Transactor[IO]): HttpRoutes[IO] = {
    import org.http4s.circe.CirceEntityDecoder._
    import org.http4s.circe.CirceEntityEncoder._
    HttpRoutes.of[IO] {

      // Create a user
      case req @ POST -> Root / "users" =>
        for {
          userCreate <- req.as[UserCreate]
          user <- createUser(userCreate, xa)
          response <- Created(user)
        } yield response

      // Get all users
      case GET -> Root / "users" =>
        for {
          users <- getUsers(xa)
          response <- Ok(users)
        } yield response

      // Get a single user by id
      case GET -> Root / "users" / IntVar(id) =>
        for {
          maybeUser <- getUser(id, xa)
          response <- maybeUser match {
                        case Some(user) => Ok(user)
                        case None => NotFound()
                      }
        } yield response

      // Update a user by id
      case req @ PUT -> Root / "users" / IntVar(id) =>
        for {
          userUpdate <- req.as[UserCreate]
          affectedRows <- updateUser(id, userUpdate, xa)
          response <- if (affectedRows > 0) Ok() else NotFound()
        } yield response

      // Delete a user by id
      case DELETE -> Root / "users" / IntVar(id) =>
        for {
          affectedRows <- deleteUser(id, xa)
          response <- if (affectedRows > 0) Ok() else NotFound()
        } yield response
    }
  }

  // Application entry point: starts the HTTP server on port 8080.
  override def run: IO[Unit] = {
    createTransactor.use { xa =>
      val httpApp = userRoutes(xa).orNotFound
      EmberServerBuilder
        .default[IO]
        .withHost("0.0.0.0")
        .withPort(8080)
        .withHttpApp(httpApp)
        .build
        .useForever
    }
  }
}
