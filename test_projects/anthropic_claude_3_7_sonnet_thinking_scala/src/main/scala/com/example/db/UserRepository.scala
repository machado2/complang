package com.example.db

import slick.jdbc.PostgresProfile.api._
import slick.lifted.ProvenShape
import com.example.model.User

import scala.concurrent.{ExecutionContext, Future}

class UserTable(tag: Tag) extends Table[User](tag, "users") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name")
  def email = column[String]("email")

  override def * : ProvenShape[User] = (id.?, name, email) <> ((User.apply _).tupled, User.unapply)
}

class UserRepository(db: Database)(implicit ec: ExecutionContext) {
  private val users = TableQuery[UserTable]

  def create(user: User): Future[User] = {
    val insertAction = (users returning users.map(_.id)) += user
    db.run(insertAction).map(id => user.copy(id = Some(id)))
  }

  def findAll(): Future[Seq[User]] = {
    db.run(users.result)
  }

  def findById(id: Int): Future[Option[User]] = {
    db.run(users.filter(_.id === id).result.headOption)
  }

  def update(id: Int, user: User): Future[Int] = {
    db.run(users.filter(_.id === id).map(u => (u.name, u.email)).update((user.name, user.email)))
  }

  def delete(id: Int): Future[Int] = {
    db.run(users.filter(_.id === id).delete)
  }
}
