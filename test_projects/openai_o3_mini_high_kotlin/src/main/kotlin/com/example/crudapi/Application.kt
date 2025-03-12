package com.example.crudapi

import io.ktor.application.*
import io.ktor.features.*
import io.ktor.http.*
import io.ktor.response.*
import io.ktor.request.*
import io.ktor.routing.*
import io.ktor.serialization.*
import io.ktor.server.engine.*
import io.ktor.server.netty.*
import kotlinx.serialization.Serializable
import org.jetbrains.exposed.sql.*
import org.jetbrains.exposed.sql.transactions.transaction
import org.jetbrains.exposed.dao.id.IntIdTable
import org.jetbrains.exposed.sql.Database
import org.jetbrains.exposed.sql.SchemaUtils

@Serializable
data class UserRequest(val name: String, val email: String)

@Serializable
data class User(val id: Int, val name: String, val email: String)

object Users : IntIdTable() {
    val name = varchar("name", 255)
    val email = varchar("email", 255)
}

fun main() {
    // Connect to PostgreSQL using the password from the environment variable PGPASSWORD.
    val pgPassword = System.getenv("PGPASSWORD") ?: ""
    Database.connect(
        url = "jdbc:postgresql://host.docker.internal:5432/complang",
        driver = "org.postgresql.Driver",
        user = "testuser",
        password = pgPassword
    )
    
    // Create the users table if it does not exist.
    transaction {
        SchemaUtils.create(Users)
    }
    
    embeddedServer(Netty, port = 8080) {
        install(ContentNegotiation) {
            json()
        }
        routing {
            route("/users") {
                // GET /users: Return all users.
                get {
                    val users = transaction {
                        Users.selectAll().map { row ->
                            User(
                                id = row[Users.id].value,
                                name = row[Users.name],
                                email = row[Users.email]
                            )
                        }
                    }
                    call.respond(HttpStatusCode.OK, users)
                }
                // POST /users: Create a new user.
                post {
                    val userRequest = call.receive<UserRequest>()
                    var newUser: User? = null
                    transaction {
                        val id = Users.insert {
                            it[name] = userRequest.name
                            it[email] = userRequest.email
                        } get Users.id
                        newUser = User(id = id.value, name = userRequest.name, email = userRequest.email)
                    }
                    call.respond(HttpStatusCode.Created, newUser!!)
                }
                // GET /users/{id}: Return a specific user.
                get("{id}") {
                    val idParam = call.parameters["id"]?.toIntOrNull()
                    if (idParam == null) {
                        call.respond(HttpStatusCode.BadRequest, "Invalid user id")
                        return@get
                    }
                    val user = transaction {
                        Users.select { Users.id eq idParam }.mapNotNull { row ->
                            User(
                                id = row[Users.id].value,
                                name = row[Users.name],
                                email = row[Users.email]
                            )
                        }.singleOrNull()
                    }
                    if (user == null) {
                        call.respond(HttpStatusCode.NotFound, "User not found")
                    } else {
                        call.respond(HttpStatusCode.OK, user)
                    }
                }
                // PUT /users/{id}: Update an existing user.
                put("{id}") {
                    val idParam = call.parameters["id"]?.toIntOrNull()
                    if (idParam == null) {
                        call.respond(HttpStatusCode.BadRequest, "Invalid user id")
                        return@put
                    }
                    val userRequest = call.receive<UserRequest>()
                    val updatedRows = transaction {
                        Users.update({ Users.id eq idParam }) {
                            it[name] = userRequest.name
                            it[email] = userRequest.email
                        }
                    }
                    if (updatedRows == 0) {
                        call.respond(HttpStatusCode.NotFound, "User not found")
                    } else {
                        val updatedUser = transaction {
                            Users.select { Users.id eq idParam }.map {
                                User(
                                    id = it[Users.id].value,
                                    name = it[Users.name],
                                    email = it[Users.email]
                                )
                            }.single()
                        }
                        call.respond(HttpStatusCode.OK, updatedUser)
                    }
                }
                // DELETE /users/{id}: Delete a user.
                delete("{id}") {
                    val idParam = call.parameters["id"]?.toIntOrNull()
                    if (idParam == null) {
                        call.respond(HttpStatusCode.BadRequest, "Invalid user id")
                        return@delete
                    }
                    val deletedRows = transaction {
                        Users.deleteWhere { Users.id eq idParam }
                    }
                    if (deletedRows == 0) {
                        call.respond(HttpStatusCode.NotFound, "User not found")
                    } else {
                        call.respond(HttpStatusCode.OK, "User deleted")
                    }
                }
            }
        }
    }.start(wait = true)
}
