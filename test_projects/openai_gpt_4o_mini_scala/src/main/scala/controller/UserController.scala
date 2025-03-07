
package controller

import model.User
import play.api.mvc._
import play.api.libs.json._
import scala.concurrent.{ExecutionContext, Future}
import javax.inject._

@Singleton
class UserController @Inject()(cc: ControllerComponents)(implicit ec: ExecutionContext) extends AbstractController(cc) {
    private var users: List[User] = List()

    implicit val userFormat: OFormat[User] = Json.format[User]

    def createUser() = Action.async(parse.json) { request =>
        val user = request.body.validate[User]
        user.fold(
            errors => Future.successful(BadRequest(Json.obj("message" -> "Invalid user data"))),
            u => {
                val newUser = u.copy(id = users.length + 1)
                users = users :+ newUser
                Future.successful(Created(Json.toJson(newUser)))
            }
        )
    }

    def listUsers() = Action.async {
        Future.successful(Ok(Json.toJson(users)))
    }

    def getUser(id: Int) = Action.async {
        users.find(_.id == id) match {
            case Some(user) => Future.successful(Ok(Json.toJson(user)))
            case None => Future.successful(NotFound(Json.obj("message" -> "User not found")))
        }
    }

    def updateUser(id: Int) = Action.async(parse.json) { request =>
        val userUpdate = request.body.validate[User]
        userUpdate.fold(
            errors => Future.successful(BadRequest(Json.obj("message" -> "Invalid user data"))),
            u => {
                users.find(_.id == id) match {
                    case Some(_) =>
                        users = users.filterNot(_.id == id) :+ u.copy(id = id)
                        Future.successful(Ok(Json.obj("message" -> "User updated")))
                    case None => Future.successful(NotFound(Json.obj("message" -> "User not found")))
                }
            }
        )
    }

    def deleteUser(id: Int) = Action.async {
        users.find(_.id == id) match {
            case Some(_) =>
                users = users.filterNot(_.id == id)
                Future.successful(NoContent)
            case None => Future.successful(NotFound(Json.obj("message" -> "User not found")))
        }
    }
}
