import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import scala.concurrent.ExecutionContext

object Main {
  implicit val system = ActorSystem()
  implicit val executionContext: ExecutionContext = system.dispatcher

  def main(args: Array[String]): Unit = {
    val routes = UserRoutes.routes
    Http().newServerAt("0.0.0.0", 8080).bind(routes)
    println("Server is running at http://0.0.0.0:8080/")
  }
}