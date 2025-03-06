package com.example

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.stereotype.Service
import org.springframework.http.ResponseEntity

@Service
class UserService @Autowired constructor(private val userRepository: UserRepository) {

    fun createUser(user: User): User {
        return userRepository.save(user)
    }

    fun getAllUsers(): List<User> {
        return userRepository.findAll()
    }

    fun getUserById(id: Long): ResponseEntity<User> {
        val user = userRepository.findById(id)
        return if (user.isPresent) {
            ResponseEntity.ok(user.get())
        } else {
            ResponseEntity.notFound().build()
        }
    }

    fun updateUser(id: Long, userUpdates: User): ResponseEntity<Void> {
        return if (userRepository.existsById(id)) {
            userUpdates.id = id
            userRepository.save(userUpdates)
            ResponseEntity.ok().build()
        } else {
            ResponseEntity.notFound().build()
        }
    }

    fun deleteUser(id: Long): ResponseEntity<Void> {
        return if (userRepository.existsById(id)) {
            userRepository.deleteById(id)
            ResponseEntity.ok().build()
        } else {
            ResponseEntity.notFound().build()
        }
    }
}