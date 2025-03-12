
package com.api.user.entity

import javax.persistence.*

@Entity
@Table(name = "users")
data class User(
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    val id: Int? = null,

    @Column(nullable = false)
    val name: String,

    @Column(nullable = false)
    val email: String
)

// DTO for create/update operations
data class UserDTO(
    val name: String,
    val email: String
)
