
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._

import scala.concurrent.Future
import scala.util.{Success, Failure}

object JsonFormats {
  implicit val userFormat = jsonFormat3(User)
  implicit val newUserFormat = jsonFormat2(NewUser)
}

import JsonFormats._

object Routes {
  import akka.http.scaladsl.server.Route
  import akka.actor.ActorSystem
  import akka.http.scaladsl.server.ExceptionHandler

  implicit def myExceptionHandler: ExceptionHandler =
    ExceptionHandler {
      case e: NoSuchElementException =>
        extractUri { uri =>
          println(s"Request to $uri could not be handled normally")
          complete(StatusCodes.NotFound)
        }
    }


  def userRoutes(system: ActorSystem): Route = {
    import system.dispatcher

    pathPrefix("users") {
      concat(
        pathEnd {
          concat(
            get {
              val usersFuture: Future[Seq[User]] = DatabaseService.getAllUsers()
              onComplete(usersFuture) {
                case Success(users) => complete(users)
                case Failure(ex) => complete(StatusCodes.InternalServerError, s"Failed to fetch users: ${ex.getMessage}")
              }
            },
            post {
              entity(as[NewUser]) { newUser =>
                val createdUserFuture: Future[User] = DatabaseService.createUser(newUser)
                onComplete(createdUserFuture) {
                  case Success(user) => complete(StatusCodes.Created, user)
                  case Failure(ex) => complete(StatusCodes.InternalServerError, s"Failed to create user: ${ex.getMessage}")
                }
              }
            }
          )
        },
        path(IntNumber) { id =>
          concat(
            get {
              val userFuture: Future[Option[User]] = DatabaseService.getUserById(id)
              onComplete(userFuture) {
                case Success(Some(user)) => complete(user)
                case Success(None) => complete(StatusCodes.NotFound)
                case Failure(ex) => complete(StatusCodes.InternalServerError, s"Failed to get user: ${ex.getMessage}")
              }
            },
            put {
              entity(as[NewUser]) { updatedUser =>
                val updateResultFuture: Future[Int] = DatabaseService.updateUser(id, updatedUser)
                onComplete(updateResultFuture) {
                  case Success(1) => complete(StatusCodes.NoContent)
                  case Success(0) => complete(StatusCodes.NotFound)
                  case Failure(ex) => complete(StatusCodes.InternalServerError, s"Failed to update user: ${ex.getMessage}")
                }
              }
            },
            delete {
              val deleteResultFuture: Future[Int] = DatabaseService.deleteUser(id)
              onComplete(deleteResultFuture) {
                case Success(1) => complete(StatusCodes.NoContent)
                case Success(0) => complete(StatusCodes.NotFound)
                case Failure(ex) => complete(StatusCodes.InternalServerError, s"Failed to delete user: ${ex.getMessage}")
              }
            }
          )
        }
      )
    }
  }
}
