import slick.jdbc.PostgresProfile.api._
import scala.concurrent.{Future, ExecutionContext}

class UserDAO(implicit ec: ExecutionContext) {
  private val users = TableQuery[Users]

  def create(user: User)(implicit db: Database): Future[User] = {
    val action = (users returning users.map(_.id)) += user
    db.run(action).map(id => user.copy(id))
  }

  def getAll(implicit db: Database): Future[List[User]] = {
    db.run(users.to[List].result)
  }

  def getById(id: Int)(implicit db: Database): Future[Option[User]] = {
    db.run(users.filter(_.id === id).result.headOption)
  }

  def update(user: User)(implicit db: Database): Future[Int] = {
    db.run(users.filter(_.id === user.id).update(user))
  }

  def delete(id: Int)(implicit db: Database): Future[Int] = {
    db.run(users.filter(_.id === id).delete)
  }
}

class Users(tag: Tag) extends Table[User](tag, "users") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name")
  def email = column[String]("email")
  def * = (id, name, email) <> (User.tupled, User.unapply)
}