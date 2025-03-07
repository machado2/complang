
import io.ktor.application.*
import io.ktor.http.*
import io.ktor.response.*
import io.ktor.routing.*
import io.ktor.serialization.jackson
import io.ktor.request.*
import kotlinx.coroutines.flow.Flow
import kotlin.collections.MutableList

// Dummy in-memory storage for users
val users = MutableList<User>(0) { User(0, "", "") }
var idCounter = 1

fun Route.userRouting() {
    route("/users") {
        post {
            val user = call.receive<User>()
            val newUser = User(idCounter++, user.name, user.email)
            users.add(newUser)
            call.respond(HttpStatusCode.Created, newUser)
        }

        get {
            call.respond(HttpStatusCode.OK, users)
        }

        get("/{id}") {
            val id = call.parameters["id"]?.toIntOrNull()
            val user = users.find { it.id == id }
            if (user != null) {
                call.respond(HttpStatusCode.OK, user)
            } else {
                call.respond(HttpStatusCode.NotFound)
            }
        }

        put("/{id}") {
            val id = call.parameters["id"]?.toIntOrNull()
            val userIndex = users.indexOfFirst { it.id == id }
            if (userIndex != -1) {
                val updatedUser = call.receive<User>()
                users[userIndex] = User(id!!, updatedUser.name, updatedUser.email)
                call.respond(HttpStatusCode.OK)
            } else {
                call.respond(HttpStatusCode.NotFound)
            }
        }

        delete("/{id}") {
            val id = call.parameters["id"]?.toIntOrNull()
            val userIndex = users.indexOfFirst { it.id == id }
            if (userIndex != -1) {
                users.removeAt(userIndex)
                call.respond(HttpStatusCode.NoContent)
            } else {
                call.respond(HttpStatusCode.NotFound)
            }
        }
    }
}
