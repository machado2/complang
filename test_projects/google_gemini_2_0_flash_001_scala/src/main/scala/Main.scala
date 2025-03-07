import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Future
import scala.io.StdIn
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Properties

case class User(id: Int, name: String, email: String)
case class NewUser(name: String, email: String)

object Main extends App {

  implicit val userFormat = jsonFormat3(User)
  implicit val newUserFormat = jsonFormat2(NewUser)

  val dbPassword = Properties.envOrNone("PGPASSWORD").getOrElse("Saloon5-Moody-Observing")

  val db = Database.forConfig("postgres",
    Map(
      "postgres.db.properties.password" -> dbPassword
    )
  )

  val usersTable = TableQuery[Users]

  class Users(tag: Tag) extends Table[User](tag, "users") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")
    def email = column[String]("email")
    def * = (id, name, email) <> (User.tupled, User.unapply)
  }

  def createUser(newUser: NewUser): Future[User] = {
    val insertAction = (usersTable returning usersTable.map(_.id)
      into ((user, id) => User(id, user.name, user.email))
    ) += NewUser(newUser.name, newUser.email)
    db.run(insertAction)
  }

  def getUsers(): Future[Seq[User]] = {
    db.run(usersTable.result)
  }

  def getUser(id: Int): Future[Option[User]] = {
    db.run(usersTable.filter(_.id === id).result.headOption)
  }

  def updateUser(id: Int, user: NewUser): Future[Int] = {
    db.run(usersTable.filter(_.id === id).map(u => (u.name, u.email)).update((user.name, user.email)))
  }

  def deleteUser(id: Int): Future[Int] = {
    db.run(usersTable.filter(_.id === id).delete)
  }

  implicit val system = ActorSystem(Behaviors.empty, "my-system")

  val route =
    pathPrefix("users") {
      concat(
        pathEnd {
          concat(
            post {
              entity(as[NewUser]) { newUser =>
                val userCreated: Future[User] = createUser(newUser)
                onComplete(userCreated) {
                  case Success(user) =>
                    complete(StatusCodes.Created, user)
                  case Failure(ex) =>
                    complete(StatusCodes.InternalServerError, s"Error creating user: ${ex.getMessage}")
                }
              }
            },
            get {
              val users: Future[Seq[User]] = getUsers()
              onComplete(users) {
                case Success(users) =>
                  complete(StatusCodes.OK, users)
                case Failure(ex) =>
                  complete(StatusCodes.InternalServerError, s"Error getting users: ${ex.getMessage}")
              }
            }
          )
        },
        path(IntNumber) { id =>
          concat(
            get {
              val user: Future[Option[User]] = getUser(id)
              onComplete(user) {
                case Success(Some(user)) =>
                  complete(StatusCodes.OK, user)
                case Success(None) =>
                  complete(StatusCodes.NotFound)
                case Failure(ex) =>
                  complete(StatusCodes.InternalServerError, s"Error getting user: ${ex.getMessage}")
              }
            },
            put {
              entity(as[NewUser]) { user =>
                val updated: Future[Int] = updateUser(id, user)
                onComplete(updated) {
                  case Success(1) =>
                    complete(StatusCodes.NoContent)
                  case Success(0) =>
                    complete(StatusCodes.NotFound)
                  case Failure(ex) =>
                    complete(StatusCodes.InternalServerError, s"Error updating user: ${ex.getMessage}")
                }
              }
            },
            delete {
              val deleted: Future[Int] = deleteUser(id)
              onComplete(deleted) {
                case Success(1) =>
                  complete(StatusCodes.NoContent)
                case Success(0) =>
                  complete(StatusCodes.NotFound)
                case Failure(ex) =>
                  complete(StatusCodes.InternalServerError, s"Error deleting user: ${ex.getMessage}")
              }
            }
          )
        }
      )
    }

  val bindingFuture = Http().newServerAt("0.0.0.0", 8080).bind(route)

  println(s"Server now online. Please navigate to http://localhost:8080/users\nPress RETURN to stop...")
  StdIn.readLine()
  bindingFuture
    .flatMap(_.unbind())
    .onComplete(_ => system.terminate())
}