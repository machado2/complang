
import play.api._
import play.api.routing._
import play.api.mvc._
import play.core.routing._
import controller.UserController
import javax.inject._

@Singleton
class App @Inject()(userController: UserController) extends ControllerHelpers {

    val router = {
        val prefix = "/users"
        Router.from {
            case POST `prefix` => userController.createUser()
            case GET `prefix` => userController.listUsers()
            case GET `prefix` / IntVar(id) => userController.getUser(id)
            case PUT `prefix` / IntVar(id) => userController.updateUser(id)
            case DELETE `prefix` / IntVar(id) => userController.deleteUser(id)
        }
    }

    def main(args: Array[String]): Unit = {
        // Start your server here
    }
}
