package com.example

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.web.bind.annotation.*
import org.springframework.http.ResponseEntity
import org.springframework.stereotype.Service
import org.springframework.beans.factory.annotation.Autowired
import javax.annotation.PostConstruct

@SpringBootApplication
class Application {
    @PostConstruct
    fun init() {
        // Initial Setup, if needed
    }

    companion object {
        @JvmStatic
        fun main(args: Array<String>) {
            runApplication<Application>(*args)
        }
    }
}

@RestController
@RequestMapping("/users")
class UserController(@Autowired private val userService: UserService) {
    @PostMapping
    fun createUser(@RequestBody user: User): ResponseEntity<User>
        = ResponseEntity.status(201).body(userService.createUser(user))

    @GetMapping
    fun getAllUsers(): List<User>
        = userService.getAllUsers()

    @GetMapping("/{id}")
    fun getUserById(@PathVariable id: Int): ResponseEntity<User>
        = userService.getUserById(id)

    @PutMapping("/{id}")
    fun updateUser(@PathVariable id: Int, @RequestBody user: User): ResponseEntity<Void>
        = userService.updateUser(id, user)

    @DeleteMapping("/{id}")
    fun deleteUser(@PathVariable id: Int): ResponseEntity<Void>
        = userService.deleteUser(id)
}

data class User(var id: Int? = null, var name: String, var email: String)

@Service
class UserService {
    private val users = mutableListOf<User>()

    fun createUser(user: User): User {
        user.id = (users.size + 1)
        users.add(user)
        return user
    }

    fun getAllUsers(): List<User> = users

    fun getUserById(id: Int): ResponseEntity<User> {
        val user = users.find { it.id == id } ?: return ResponseEntity.notFound().build()
        return ResponseEntity.ok(user)
    }

    fun updateUser(id: Int, userUpdates: User): ResponseEntity<Void> {
        val user = users.find { it.id == id } ?: return ResponseEntity.notFound().build()
        user.name = userUpdates.name
        user.email = userUpdates.email
        return ResponseEntity.ok().build()
    }

    fun deleteUser(id: Int): ResponseEntity<Void> {
        val user = users.find { it.id == id } ?: return ResponseEntity.notFound().build()
        users.remove(user)
        return ResponseEntity.ok().build()
    }
}