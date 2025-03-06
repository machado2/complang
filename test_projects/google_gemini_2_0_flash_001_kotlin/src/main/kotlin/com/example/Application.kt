package com.example

import io.ktor.server.application.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import io.ktor.server.netty.*
import io.ktor.serialization.kotlinx.json.*
import io.ktor.server.plugins.contentnegotiation.*
import io.ktor.server.request.*
import io.ktor.http.*
import kotlinx.serialization.*
import kotlinx.serialization.json.*
import io.ktor.server.plugins.statuspages.*
import io.ktor.server.plugins.* // Necessary import for StatusPages

import org.jetbrains.exposed.sql.*
import org.jetbrains.exposed.sql.transactions.*
import org.jetbrains.exposed.sql.transactions.experimental.newSuspendedTransaction
import org.jetbrains.exposed.dao.id.IntIdTable


fun main(args: Array<String>): Unit = EngineMain.main(args)

@Serializable
data class User(val id: Int, val name: String, val email: String)

@Serializable
data class UserCreate(val name: String, val email: String)


object Users : IntIdTable() {
    val name = text("name")
    val email = text("email")
}


fun Application.module() {
    install(ContentNegotiation) {
        json(Json {
            prettyPrint = true
            isLenient = true
        })
    }

    install(StatusPages) {
        exception<Throwable> {
            call.respondText(text = "500: Internal Server Error", status = HttpStatusCode.InternalServerError)
        }
        status(HttpStatusCode.NotFound) {
            call.respondText(text = "404: Not Found", status = HttpStatusCode.NotFound)
        }
    }

    val pgPassword = System.getenv("PGPASSWORD") ?: "Saloon5-Moody-Observing"
    val dbConfig = Database.connect(
        "jdbc:postgresql://host.docker.internal:5432/complang",
        driver = "org.postgresql.Driver",
        user = "testuser",
        password = pgPassword
    )

    routing {
        get("/") {
            call.respondText("Hello, world!")
        }

        route("/users") {
            post {
                val userCreate = call.receive<UserCreate>()
                val id: Int? = transaction {
                    val insertStatement = Users.insert {
                        it[name] = userCreate.name
                        it[email] = userCreate.email
                    }
                    insertStatement.resultedValues?.get(0)?.get(Users.id)?.value

                }

                if (id != null) {
                    val user = User(id, userCreate.name, userCreate.email)
                    call.respond(HttpStatusCode.Created, user)
                } else {
                    call.respond(HttpStatusCode.InternalServerError)
                }
            }

            get {
                val users = transaction {
                    Users.selectAll().map { User(it[Users.id].value, it[Users.name], it[Users.email]) }
                }
                call.respond(users)
            }

            get("{id}") {
                val id = call.parameters["id"]?.toIntOrNull()
                if (id == null) {
                    call.respond(HttpStatusCode.BadRequest, "Invalid ID format")
                    return@get
                }

                val user = transaction {
                    Users.select { Users.id eq id }.map { User(it[Users.id].value, it[Users.name], it[Users.email]) }
                        .singleOrNull()
                }

                if (user == null) {
                    call.respond(HttpStatusCode.NotFound)
                } else {
                    call.respond(user)
                }
            }

            put("{id}") {

                val id = call.parameters["id"]?.toIntOrNull()
                if (id == null) {
                    call.respond(HttpStatusCode.BadRequest, "Invalid ID format")
                    return@put
                }

                val userUpdate = call.receive<UserCreate>()
                val updated = transaction {
                    Users.update({ Users.id eq id }) {
                        it[name] = userUpdate.name
                        it[email] = userUpdate.email
                    }
                }

                if (updated > 0) {
                    call.respond(HttpStatusCode.NoContent)
                } else {
                    call.respond(HttpStatusCode.NotFound)
                }
            }

            delete("{id}") {
                val id = call.parameters["id"]?.toIntOrNull()
                if (id == null) {
                    call.respond(HttpStatusCode.BadRequest, "Invalid ID format")
                    return@delete
                }

                val deleted = transaction {
                    Users.deleteWhere { Users.id eq id }
                }

                if (deleted > 0) {
                    call.respond(HttpStatusCode.NoContent)
                } else {
                    call.respond(HttpStatusCode.NotFound)
                }
            }
        }
    }
}
