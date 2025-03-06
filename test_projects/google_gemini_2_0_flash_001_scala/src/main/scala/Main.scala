import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.{StatusCodes, HttpResponse}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._
import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn

import slick.jdbc.PostgresProfile.api._
import scala.util.{Success, Failure}

// JSON support using spray-json
trait JsonSupport extends SprayJsonSupport {
  import DefaultJsonProtocol._
  implicit val userFormat = jsonFormat3(User)
}

// Case class representing a user
case class User(id: Int, name: String, email: String)

object Main extends App with JsonSupport {

  implicit val system = ActorSystem("ComplangCRUD")
  implicit val executionContext = system.dispatcher

  // Database configuration
  val db = Database.forConfig("postgres")
  val usersTable = TableQuery[Users]

  // Definition of the Users table
  class Users(tag: Tag) extends Table[User](tag, "users") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")
    def email = column[String]("email")
    def * = (id, name, email) <> (User.tupled, User.unapply)
  }

  // Create the table if it doesn't exist
  val setup = DBIO.seq(
    usersTable.schema.createIfNotExists
  )

  db.run(setup).onComplete {
    case Success(_) => println("Table created (if it didn't exist)")
    case Failure(e) => println(s"Failed to create table: ${e.getMessage}")
  }

  // Route definition
  val route = {
    pathPrefix("users") {
      concat(
        pathEnd {
          concat(
            post {
              entity(as[User]) { user =>
                val insertAction = (usersTable returning usersTable.map(_.id)) += user
                val inserted: Future[Int] = db.run(insertAction)
                onComplete(inserted) {
                  case Success(newId) =>
                    complete((StatusCodes.Created, User(newId, user.name, user.email)))
                  case Failure(e) =>
                    println(s"Failed to insert user: ${e.getMessage}")
                    complete(StatusCodes.InternalServerError)
                }
              }
            },
            get {
              val users: Future[Seq[User]] = db.run(usersTable.result)
              onComplete(users) {
                case Success(users) => complete(users)
                case Failure(e) =>
                  println(s"Failed to get users: ${e.getMessage}")
                  complete(StatusCodes.InternalServerError)
              }
            }
          )
        },
        path(IntNumber) {
          id =>
            concat(
              get {
                val user: Future[Option[User]] = db.run(usersTable.filter(_.id === id).result.headOption)
                onComplete(user) {
                  case Success(Some(user)) => complete(user)
                  case Success(None) => complete(StatusCodes.NotFound)
                  case Failure(e) =>
                    println(s"Failed to get user: ${e.getMessage}")
                    complete(StatusCodes.InternalServerError)
                }
              },
              put {
                entity(as[User]) { user =>
                  val updateAction = usersTable.filter(_.id === id).map(u => (u.name, u.email)).update((user.name, user.email))
                  val updated: Future[Int] = db.run(updateAction)
                  onComplete(updated) {
                    case Success(1) => complete(StatusCodes.NoContent)
                    case Success(0) => complete(StatusCodes.NotFound)
                    case Failure(e) =>
                      println(s"Failed to update user: ${e.getMessage}")
                      complete(StatusCodes.InternalServerError)
                  }
                }
              },
              delete {
                val deleteAction = usersTable.filter(_.id === id).delete
                val deleted: Future[Int] = db.run(deleteAction)
                onComplete(deleted) {
                  case Success(1) => complete(StatusCodes.NoContent)
                  case Success(0) => complete(StatusCodes.NotFound)
                  case Failure(e) =>
                    println(s"Failed to delete user: ${e.getMessage}")
                    complete(StatusCodes.InternalServerError)
                }
              }
            )
        }
      )
    }
  }

  // Start the server
  val bindingFuture = Http().newServerAt("0.0.0.0", 8080).bind(route)

  println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
  StdIn.readLine()
  bindingFuture
    .flatMap(_.unbind())
    .onComplete(_ => system.terminate())
}
