
import slick.jdbc.PostgresProfile.api._
import slick.jdbc.JdbcBackend.Database
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object DatabaseService {
  val db = Database.forConfig("slick.dbs.default")
  val usersTable = TableQuery[UsersTable]

  def createTableIfNotExists(): Future[Unit] = {
    db.run(usersTable.schema.createIfNotExists)
  }

  def getAllUsers(): Future[Seq[User]] = {
    db.run(usersTable.result)
  }

  def getUserById(id: Int): Future[Option[User]] = {
    db.run(usersTable.filter(_.id === id).result.headOption)
  }

  def createUser(newUser: NewUser): Future[User] = {
    import scala.concurrent.Await
    import scala.concurrent.duration._

    val idFuture: Future[Int] = db.run((usersTable returning usersTable.map(_.id)) += User(0, newUser.name, newUser.email))
    val id = Await.result(idFuture, 5.seconds)

    Future {
      User(id, newUser.name, newUser.email)
    }
  }


  def updateUser(id: Int, updatedUser: NewUser): Future[Int] = {
    db.run(usersTable.filter(_.id === id).map(u => (u.name, u.email)).update((updatedUser.name, updatedUser.email)))
  }

  def deleteUser(id: Int): Future[Int] = {
    db.run(usersTable.filter(_.id === id).delete)
  }
}
