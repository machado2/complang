package complang

import doobie._
import doobie.implicits._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

case class User(id: Int, name: String, email: String)

object UserRepository {
  implicit val userDecoder: Decoder[User] = deriveDecoder[User]
  implicit val userEncoder: Encoder[User] = deriveEncoder[User]

  def getAllUsers: ConnectionIO[List[User]] = sql"SELECT * FROM users".query[User].to[List]

  def getUserById(id: Int): ConnectionIO[Option[User]] = sql"SELECT * FROM users WHERE id = $id".query[User].option

  def createUser(user: User): ConnectionIO[Int] = sql"INSERT INTO users (name, email) VALUES (${user.name}, ${user.email})".update.run

  def updateUser(id: Int, user: User): ConnectionIO[Int] = sql"UPDATE users SET name = ${user.name}, email = ${user.email} WHERE id = $id".update.run

  def deleteUser(id: Int): ConnectionIO[Int] = sql"DELETE FROM users WHERE id = $id".update.run
}
