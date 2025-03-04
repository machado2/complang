
import io.ktor.server.application.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import io.ktor.server.netty.*
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import io.ktor.server.plugins.contentnegotiation.*
import io.ktor.serialization.kotlinx.json.*
import io.ktor.server.request.*
import java.sql.DriverManager
import io.ktor.http.*

@Serializable
data class User(val id: Int, val name: String, val email: String)

fun main(args: Array<String>): Unit = EngineMain.main(args)

fun Application.module() {

    install(ContentNegotiation) {
        json(Json {
            prettyPrint = true
            isLenient = true
        })
    }

    val jdbcUrl = "jdbc:postgresql://host.docker.internal:5432/test_google_gemini_2_0_flash_001_kotlin"
    val dbUser = "postgres"
    val dbPassword = System.getenv("PGPASSWORD") ?: ""

    val connection = DriverManager.getConnection(jdbcUrl, dbUser, dbPassword)

    // Check if table exists and create if not
    connection.use {
        val tableExists = it.metaData.getTables(null, null, "users", null).next()
        if (!tableExists) {
            val createTableStatement = it.createStatement()
            createTableStatement.executeUpdate(
                """
                CREATE TABLE users (
                    id SERIAL PRIMARY KEY,
                    name TEXT,
                    email TEXT
                )
                """
            )
            println("Table 'users' created.")
        }
    }

    routing {
        get("/") {
            call.respondText("Hello, world!")
        }

        route("/users") {
            post {
                val user = call.receive<User>()
                connection.use {
                    val statement = it.prepareStatement("INSERT INTO users (name, email) VALUES (?, ?) RETURNING id")
                    statement.setString(1, user.name)
                    statement.setString(2, user.email)
                    val result = statement.executeQuery()
                    if (result.next()) {
                        val id = result.getInt("id")
                        val newUser = User(id, user.name, user.email)
                        call.respond(HttpStatusCode.Created, newUser)
                    } else {
                        call.respond(HttpStatusCode.InternalServerError)
                    }
                }
            }

            get {
                val users = mutableListOf<User>()
                connection.use {
                    val statement = it.createStatement()
                    val result = statement.executeQuery("SELECT id, name, email FROM users")
                    while (result.next()) {
                        val id = result.getInt("id")
                        val name = result.getString("name")
                        val email = result.getString("email")
                        users.add(User(id, name, email))
                    }
                }
                call.respond(users)
            }

            get("/{id}") {
                val id = call.parameters["id"]?.toIntOrNull() ?: return@get call.respond(HttpStatusCode.BadRequest)
                connection.use {
                    val statement = it.prepareStatement("SELECT id, name, email FROM users WHERE id = ?")
                    statement.setInt(1, id)
                    val result = statement.executeQuery()
                    if (result.next()) {
                        val name = result.getString("name")
                        val email = result.getString("email")
                        val user = User(id, name, email)
                        call.respond(user)
                    } else {
                        call.respond(HttpStatusCode.NotFound)
                    }
                }
            }

            put("/{id}") {
                val id = call.parameters["id"]?.toIntOrNull() ?: return@put call.respond(HttpStatusCode.BadRequest)
                val user = call.receive<User>()
                connection.use {
                    val statement = it.prepareStatement("UPDATE users SET name = ?, email = ? WHERE id = ?")
                    statement.setString(1, user.name)
                    statement.setString(2, user.email)
                    statement.setInt(3, id)
                    val updatedRows = statement.executeUpdate()
                    if (updatedRows > 0) {
                        call.respond(HttpStatusCode.NoContent)
                    } else {
                        call.respond(HttpStatusCode.NotFound)
                    }
                }
            }

            delete("/{id}") {
                val id = call.parameters["id"]?.toIntOrNull() ?: return@delete call.respond(HttpStatusCode.BadRequest)
                connection.use {
                    val statement = it.prepareStatement("DELETE FROM users WHERE id = ?")
                    statement.setInt(1, id)
                    val deletedRows = statement.executeUpdate()
                    if (deletedRows > 0) {
                        call.respond(HttpStatusCode.NoContent)
                    } else {
                        call.respond(HttpStatusCode.NotFound)
                    }
                }
            }
        }
    }
}
