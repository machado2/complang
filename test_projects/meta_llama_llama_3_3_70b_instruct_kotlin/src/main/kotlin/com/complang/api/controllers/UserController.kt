
package com.complang.api.controllers

import com.complang.api.models.User
import com.complang.api.services.UserService
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/users")
class UserController {
    @Autowired
    private lateinit var userService: UserService

    @PostMapping
    fun createUser(@RequestBody user: User): ResponseEntity<User> {
        val createdUser = userService.createUser(user)
        return ResponseEntity.status(HttpStatus.CREATED).body(createdUser)
    }

    @GetMapping
    fun getUsers(): ResponseEntity<List<User>> {
        val users = userService.getUsers()
        return ResponseEntity.ok(users)
    }

    @GetMapping("/{id}")
    fun getUser(@PathVariable id: Int): ResponseEntity<User> {
        val user = userService.getUser(id)
        if (user != null) {
            return ResponseEntity.ok(user)
        }
        return ResponseEntity.notFound().build()
    }

    @PutMapping("/{id}")
    fun updateUser(@PathVariable id: Int, @RequestBody user: User): ResponseEntity<User> {
        val updatedUser = userService.updateUser(id, user)
        if (updatedUser != null) {
            return ResponseEntity.ok(updatedUser)
        }
        return ResponseEntity.notFound().build()
    }

    @DeleteMapping("/{id}")
    fun deleteUser(@PathVariable id: Int): ResponseEntity<Void> {
        userService.deleteUser(id)
        return ResponseEntity.noContent().build()
    }
}
