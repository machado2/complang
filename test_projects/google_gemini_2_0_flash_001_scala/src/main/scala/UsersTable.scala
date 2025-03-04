
import slick.jdbc.PostgresProfile.api._
import slick.lifted.Tag

class UsersTable(tag: Tag) extends Table[User](tag, "users") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name")
  def email = column[String]("email")
  def * = (id, name, email) <> (User.tupled, User.unapply)
}
