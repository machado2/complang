
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import spray.json._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport

import scala.io.StdIn
import slick.jdbc.PostgresProfile.api._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.Await
import scala.concurrent.duration._
import java.sql.Timestamp
import java.time.Instant

case class User(id: Int, name: String, email: String)
case class NewUser(name: String, email: String)

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val userFormat = jsonFormat3(User)
  implicit val newUserFormat = jsonFormat2(NewUser)
}

object Main extends JsonSupport {

  def main(args: Array[String]) {

    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher

    val db = Database.forConfig("postgres")

    val pgUser = System.getenv("PGUSER")
    val pgPassword = System.getenv("PGPASSWORD")

    object Users extends Table[(Int, String, String)]("users") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def name = column[String]("name")
      def email = column[String]("email")
      def * = (id, name, email)
      def autoInc = (name, email) <> ( (t:(String, String)) => NewUser(t._1, t._2), (u:NewUser) => Some((u.name, u.email)) )
    }

    val setup = DBIO.seq(
      Users.schema.create
    )

    val setupFuture = db.run(setup).recover {
      case ex: Exception => println("Database already exists, skipping schema creation.")
    }

    Await.result(setupFuture, 10.seconds) // Wait for schema creation

    val route =
      pathPrefix("users") {
        concat(
          pathEnd {
            concat(
              get {
                val usersFuture: Future[Seq[(Int, String, String)]] = db.run(Users.result)
                val usersMapped: Future[Seq[User]] = usersFuture.map(_.map(u => User(u._1, u._2, u._3)))
                onComplete(usersMapped) {
                  case Success(users) => complete(StatusCodes.OK, users)
                  case Failure(e) => complete(StatusCodes.InternalServerError, s"An error occurred: ${e.getMessage}")
                }
              },
              post {
                entity(as[NewUser]) { newUser =>
                  val insertAction = Users.map(u => (u.name, u.email)) += (newUser.name, newUser.email)
                  val insertFuture: Future[Int] = db.run(insertAction)

                  val selectFuture: Future[Seq[(Int, String, String)]] = insertFuture.flatMap { _ =>
                     db.run(Users.filter(user => user.name === newUser.name && user.email === newUser.email).result)
                  }

                  val userMapped: Future[Option[User]] = selectFuture.map(_.headOption.map(u => User(u._1, u._2, u._3)))


                  onComplete(userMapped) {
                    case Success(Some(user)) => complete(StatusCodes.Created, user)
                    case Success(None) => complete(StatusCodes.InternalServerError, "Failed to retrieve created user")
                    case Failure(e) => complete(StatusCodes.InternalServerError, s"An error occurred: ${e.getMessage}")
                  }
                }
              }
            )
          },
          path(IntNumber) { id =>
            concat(
              get {
                val userFuture: Future[Seq[(Int, String, String)]] = db.run(Users.filter(_.id === id).result)
                val userMapped: Future[Option[User]] = userFuture.map(_.headOption.map(u => User(u._1, u._2, u._3)))
                onComplete(userMapped) {
                  case Success(Some(user)) => complete(StatusCodes.OK, user)
                  case Success(None) => complete(StatusCodes.NotFound)
                  case Failure(e) => complete(StatusCodes.InternalServerError, s"An error occurred: ${e.getMessage}")
                }
              },
              put {
                entity(as[NewUser]) { updatedUser =>
                  val updateAction: DBIO[Int] = Users.filter(_.id === id).map(u => (u.name, u.email)).update((updatedUser.name, updatedUser.email))
                  val updateFuture: Future[Int] = db.run(updateAction)

                  onComplete(updateFuture) {
                    case Success(1) => complete(StatusCodes.NoContent)
                    case Success(0) => complete(StatusCodes.NotFound)
                    case Failure(e) => complete(StatusCodes.InternalServerError, s"An error occurred: ${e.getMessage}")
                  }
                }
              },
              delete {
                val deleteAction: DBIO[Int] = Users.filter(_.id === id).delete
                val deleteFuture: Future[Int] = db.run(deleteAction)

                onComplete(deleteFuture) {
                  case Success(1) => complete(StatusCodes.NoContent)
                  case Success(0) => complete(StatusCodes.NotFound)
                  case Failure(e) => complete(StatusCodes.InternalServerError, s"An error occurred: ${e.getMessage}")
                }
              }
            )
          }
        )
      }

    val bindingFuture = Http().bindAndHandle(route, "0.0.0.0", 8080)

    println(s"Server online at http://localhost:8080/
Press RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}
