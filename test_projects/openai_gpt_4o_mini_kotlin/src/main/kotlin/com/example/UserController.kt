package com.example

import org.springframework.web.bind.annotation.*
import org.springframework.http.ResponseEntity
import org.springframework.beans.factory.annotation.Autowired

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
    fun getUserById(@PathVariable id: Long): ResponseEntity<User>
        = userService.getUserById(id)

    @PutMapping("/{id}")
    fun updateUser(@PathVariable id: Long, @RequestBody user: User): ResponseEntity<Void>
        = userService.updateUser(id, user)

    @DeleteMapping("/{id}")
    fun deleteUser(@PathVariable id: Long): ResponseEntity<Void>
        = userService.deleteUser(id)
}