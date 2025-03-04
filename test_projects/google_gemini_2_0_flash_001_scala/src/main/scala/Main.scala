
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import scala.io.StdIn

object Main extends App {
  implicit val system = ActorSystem("user-api")
  implicit val executionContext = system.dispatcher

  DatabaseService.createTableIfNotExists()

  val routes: Route = Routes.userRoutes(system)

  val bindingFuture = Http().newServerAt("0.0.0.0", 8080).bind(routes)

  println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
  StdIn.readLine() // let it run until user presses return
  bindingFuture
    .flatMap(_.unbind()) // trigger unbinding from the port
    .onComplete(_ => system.terminate()) // and shutdown when done
}
