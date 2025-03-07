package com.example.complang.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import com.example.complang.model.User;

public interface UserRepository extends JpaRepository<User, Long> {
}
