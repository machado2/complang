
package com.example

import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.*
import org.springframework.jdbc.core.JdbcTemplate
import org.springframework.web.server.ResponseStatusException
import org.springframework.dao.EmptyResultDataAccessException
import org.springframework.http.MediaType
import java.net.URI

@RestController
@RequestMapping("/users", produces = [MediaType.APPLICATION_JSON_VALUE])
class UserController(private val jdbcTemplate: JdbcTemplate) {

    @PostMapping
    fun createUser(@RequestBody user: User): ResponseEntity<User> {
        val sql = "INSERT INTO users (name, email) VALUES (?, ?) RETURNING id"
        val id = jdbcTemplate.queryForObject(sql, arrayOf(user.name, user.email), Int::class.java)
            ?: throw IllegalStateException("Failed to insert user")

        val createdUser = User(id, user.name, user.email)
        return ResponseEntity.created(URI.create("/users/$id")).body(createdUser)
    }

    @GetMapping
    fun getAllUsers(): List<User> {
        val sql = "SELECT id, name, email FROM users"
        return jdbcTemplate.query(sql) { rs, _ ->
            User(rs.getInt("id"), rs.getString("name"), rs.getString("email"))
        }
    }

    @GetMapping("/{id}")
    fun getUserById(@PathVariable id: Int): User {
        val sql = "SELECT id, name, email FROM users WHERE id = ?"
        return try {
            jdbcTemplate.queryForObject(sql, arrayOf(id)) { rs, _ ->
                User(rs.getInt("id"), rs.getString("name"), rs.getString("email"))
            } ?: throw ResponseStatusException(HttpStatus.NOT_FOUND, "User not found")
        } catch (e: EmptyResultDataAccessException) {
            throw ResponseStatusException(HttpStatus.NOT_FOUND, "User not found")
        }
    }

    @PutMapping("/{id}")
    fun updateUser(@PathVariable id: Int, @RequestBody user: User): ResponseEntity<Any> {
        val sql = "UPDATE users SET name = ?, email = ? WHERE id = ?"
        val rowsAffected = jdbcTemplate.update(sql, user.name, user.email, id)
        return if (rowsAffected > 0) {
            ResponseEntity.noContent().build()
        } else {
            throw ResponseStatusException(HttpStatus.NOT_FOUND, "User not found")
        }
    }

    @DeleteMapping("/{id}")
    fun deleteUser(@PathVariable id: Int): ResponseEntity<Any> {
        val sql = "DELETE FROM users WHERE id = ?"
        val rowsAffected = jdbcTemplate.update(sql, id)
        return if (rowsAffected > 0) {
            ResponseEntity.noContent().build()
        } else {
            throw ResponseStatusException(HttpStatus.NOT_FOUND, "User not found")
        }
    }
}
