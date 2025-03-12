package com.example.model

import spray.json._
import DefaultJsonProtocol._

case class User(id: Option[Int] = None, name: String, email: String)

object UserJsonProtocol extends DefaultJsonProtocol {
  implicit val userFormat: RootJsonFormat[User] = jsonFormat3(User.apply)
}
