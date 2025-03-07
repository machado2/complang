
package com.example

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.*
import javax.persistence.*
import org.springframework.data.jpa.repository.JpaRepository

@SpringBootApplication
class Application

fun main(args: Array<String>) {
    runApplication<Application>(*args)
}

@Entity
@Table(name = "users")
data class User(
    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Int? = null,
    var name: String = "",
    var email: String = ""
)

interface UserRepository : JpaRepository<User, Int>

@RestController
@RequestMapping("/users")
class UserController(private val userRepository: UserRepository) {

    @PostMapping
    fun createUser(@RequestBody user: User): ResponseEntity<User> {
        val savedUser = userRepository.save(user)
        return ResponseEntity.status(HttpStatus.CREATED).body(savedUser)
    }

    @GetMapping
    fun getAllUsers(): List<User> {
        return userRepository.findAll()
    }

    @GetMapping("/{id}")
    fun getUserById(@PathVariable("id") id: Int): ResponseEntity<User> {
        return userRepository.findById(id)
            .map { ResponseEntity.ok(it) }
            .orElse(ResponseEntity.notFound().build())
    }

    @PutMapping("/{id}")
    fun updateUser(@PathVariable("id") id: Int, @RequestBody user: User): ResponseEntity<ResponseEntity<User>> {
        return userRepository.findById(id)
            .map { existingUser ->
                val updatedUser = existingUser.copy(name = user.name, email = user.email)
                userRepository.save(updatedUser)
                ResponseEntity.ok<User>().build()
            }
            .orElse(ResponseEntity.notFound().build())
    }

    @DeleteMapping("/{id}")
    fun deleteUser(@PathVariable("id") id: Int): ResponseEntity<Void> {
        return userRepository.findById(id)
            .map {
                userRepository.deleteById(id)
                ResponseEntity.noContent().build<Void>()
            }
            .orElse(ResponseEntity.notFound().build())
    }
}
