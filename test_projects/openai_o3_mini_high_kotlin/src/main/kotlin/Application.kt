import io.ktor.server.engine.embeddedServer
import io.ktor.server.netty.Netty
import io.ktor.server.application.*
import io.ktor.server.response.*
import io.ktor.server.request.*
import io.ktor.http.*
import io.ktor.server.routing.*
import io.ktor.server.plugins.contentnegotiation.ContentNegotiation
import io.ktor.serialization.gson.gson
import org.jetbrains.exposed.sql.*
import org.jetbrains.exposed.sql.transactions.transaction
import org.jetbrains.exposed.sql.SqlExpressionBuilder.eq

object Users : Table("users") {
    val id = integer("id").autoIncrement()
    val name = text("name")
    val email = text("email")
    override val primaryKey = PrimaryKey(id)
}

data class User(val id: Int, val name: String, val email: String)
data class NewUser(val name: String, val email: String)

fun main() {
    val dbPassword = System.getenv("PGPASSWORD") ?: "default"
    Database.connect(
        url = "jdbc:postgresql://host.docker.internal:5432/complang",
        driver = "org.postgresql.Driver",
        user = "testuser",
        password = dbPassword
    )
    
    embeddedServer(Netty, port = 8080) {
        install(ContentNegotiation) {
            gson {
                setPrettyPrinting()
            }
        }
        routing {
            route("/users") {
                // POST /users: Create a user
                post {
                    val newUser = call.receive<NewUser>()
                    val insertedId = transaction {
                        Users.insert {
                            it[name] = newUser.name
                            it[email] = newUser.email
                        } get Users.id
                    }
                    val user = User(id = insertedId, name = newUser.name, email = newUser.email)
                    call.respond(HttpStatusCode.Created, user)
                }
                // GET /users: Retrieve all users
                get {
                    val users = transaction {
                        Users.selectAll().map { row ->
                            User(
                                id = row[Users.id],
                                name = row[Users.name],
                                email = row[Users.email]
                            )
                        }
                    }
                    call.respond(HttpStatusCode.OK, users)
                }
                // GET /users/{id}: Retrieve a single user
                get("{id}") {
                    val idParam = call.parameters["id"]?.toIntOrNull()
                    if (idParam == null) {
                        call.respond(HttpStatusCode.BadRequest, "Invalid id")
                        return@get
                    }
                    val user = transaction {
                        Users.select { Users.id eq idParam }
                            .mapNotNull {
                                User(
                                    id = it[Users.id],
                                    name = it[Users.name],
                                    email = it[Users.email]
                                )
                            }
                            .singleOrNull()
                    }
                    if (user == null) {
                        call.respond(HttpStatusCode.NotFound)
                    } else {
                        call.respond(HttpStatusCode.OK, user)
                    }
                }
                // PUT /users/{id}: Update a user
                put("{id}") {
                    val idParam = call.parameters["id"]?.toIntOrNull()
                    if (idParam == null) {
                        call.respond(HttpStatusCode.BadRequest, "Invalid id")
                        return@put
                    }
                    val updateUser = call.receive<NewUser>()
                    val updatedRows = transaction {
                        Users.update(where = { Users.id eq idParam }) {
                            it[name] = updateUser.name
                            it[email] = updateUser.email
                        }
                    }
                    if (updatedRows == 0) {
                        call.respond(HttpStatusCode.NotFound)
                    } else {
                        call.respond(HttpStatusCode.OK)
                    }
                }
                // DELETE /users/{id}: Delete a user
                delete("{id}") {
                    val idParam = call.parameters["id"]?.toIntOrNull()
                    if (idParam == null) {
                        call.respond(HttpStatusCode.BadRequest, "Invalid id")
                        return@delete
                    }
                    val deletedRows = transaction {
                        Users.deleteWhere { Users.id eq idParam }
                    }
                    if (deletedRows == 0) {
                        call.respond(HttpStatusCode.NotFound)
                    } else {
                        call.respond(HttpStatusCode.OK)
                    }
                }
            }
        }
    }.start(wait = true)
}