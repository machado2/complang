import slick.jdbc.PostgresProfile.api._

object Tables {

  // Definition of the Users table
  class Users(tag: Tag) extends Table[User](tag, "users") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")
    def email = column[String]("email")
    def * = (id, name, email) <> (User.tupled, User.unapply)
  }

  lazy val usersTable = TableQuery[Users]

}

case class User(id: Int, name: String, email: String)