package complang

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse, StatusCode}
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import io.circe.syntax._

object UserController {
  implicit val system = ActorSystem("complang")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  val routes = pathPrefix("users") {
    pathEndOrSingleSlash {
      get {
        complete(UserRepository.getAllUsers.map(_.asJson))
      } ~
      post {
        entity(as[String]) { json =>
          val user = json.fromJson[User].getOrElse(throw new Exception("Invalid JSON"))
          complete(UserRepository.createUser(user).map(_ => StatusCode(201) -> User(user.id, user.name, user.email).asJson))
        }
      }
    } ~
    path(IntNumber) { id =>
      get {
        complete(UserRepository.getUserById(id).map(_.map(_.asJson)))
      } ~
      put {
        entity(as[String]) { json =>
          val user = json.fromJson[User].getOrElse(throw new Exception("Invalid JSON"))
          complete(UserRepository.updateUser(id, user).map(_ => StatusCode(200) -> "".asJson))
        }
      } ~
      delete {
        complete(UserRepository.deleteUser(id).map(_ => StatusCode(204)))
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val bindingFuture = Http().bindAndHandle(routes, "localhost", 8080)
    println(s"Server online at http://localhost:8080/")
  }
}
