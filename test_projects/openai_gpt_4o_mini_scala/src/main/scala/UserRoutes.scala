import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import scala.concurrent.ExecutionContext

class UserRoutes(userDAO: UserDAO)(implicit ec: ExecutionContext) {
  val routes: Route = pathPrefix("users") {
    concat(
      post { // Create user
        entity(as[User]) { user =>
          onSuccess(userDAO.create(user)) { createdUser =>
            complete((201, createdUser))
          }
        }
      },
      get { // Get all users
        onSuccess(userDAO.getAll()) { users =>
          complete(users)
        }
      },
      path("" / IntNumber) { id => // Get user by ID
        get {
          onSuccess(userDAO.getById(id)) { userOpt =>
            userOpt match {
              case Some(user) => complete(user)
              case None => complete((404, "User not found"))
            }
          }
        } ~
        put { // Update user
          entity(as[User]) { user =>
            onSuccess(userDAO.update(user.copy(id = id))) { rowsAffected =>
              if (rowsAffected > 0) complete((200, "User updated"))
              else complete((404, "User not found"))
            }
          }
        } ~
        delete { // Delete user
          onSuccess(userDAO.delete(id)) { rowsAffected =>
            if (rowsAffected > 0) complete((200, "User deleted"))
            else complete((404, "User not found"))
          }
        }
      }
    )
  }
}