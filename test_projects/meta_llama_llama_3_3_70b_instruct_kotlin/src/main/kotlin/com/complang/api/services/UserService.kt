
package com.complang.api.services

import com.complang.api.models.User
import com.complang.api.repositories.UserRepository
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.stereotype.Service

@Service
class UserService {
    @Autowired
    private lateinit var userRepository: UserRepository

    fun createUser(user: User): User {
        return userRepository.save(user)
    }

    fun getUsers(): List<User> {
        return userRepository.findAll()
    }

    fun getUser(id: Int): User? {
        return userRepository.findById(id).orElse(null)
    }

    fun updateUser(id: Int, user: User): User? {
        val existingUser = getUser(id)
        if (existingUser != null) {
            existingUser.name = user.name
            existingUser.email = user.email
            return userRepository.save(existingUser)
        }
        return null
    }

    fun deleteUser(id: Int) {
        userRepository.deleteById(id)
    }
}
