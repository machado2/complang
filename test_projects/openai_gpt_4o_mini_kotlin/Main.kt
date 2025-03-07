
import io.ktor.application.*
import io.ktor.routing.*
import io.ktor.server.engine.embeddedServer
import io.ktor.server.netty.Netty
import io.ktor.features.ContentNegotiation
import io.ktor.serialization.jackson
import io.ktor.features.StatusPages

fun main() {
    embeddedServer(Netty, port = 8080, module = Application::module).start(wait = true)
}

fun Application.module() {
    install(ContentNegotiation) {
        jackson {}
    }

    install(StatusPages) {
        // Handle exceptions if needed
    }
    
    routing {
        userRouting()
    }
}
