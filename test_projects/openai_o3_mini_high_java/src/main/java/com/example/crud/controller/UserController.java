package com.example.crud.controller;

import com.example.crud.model.User;
import com.example.crud.repository.UserRepository;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("/users")
public class UserController {

  private final UserRepository userRepository;

  public UserController(UserRepository userRepository) {
    this.userRepository = userRepository;
  }

  // POST /users: Create a user
  @PostMapping
  public ResponseEntity<User> createUser(@RequestBody User user) {
    User createdUser = userRepository.save(user);
    return ResponseEntity.status(201).body(createdUser);
  }

  // GET /users: Get all users
  @GetMapping
  public ResponseEntity<List<User>> getAllUsers() {
    List<User> users = userRepository.findAll();
    return ResponseEntity.ok(users);
  }

  // GET /users/{id}: Get a single user
  @GetMapping("/{id}")
  public ResponseEntity<User> getUserById(@PathVariable Integer id) {
    Optional<User> userOpt = userRepository.findById(id);
    return userOpt.map(ResponseEntity::ok)
                  .orElseGet(() -> ResponseEntity.notFound().build());
  }

  // PUT /users/{id}: Update a user
  @PutMapping("/{id}")
  public ResponseEntity<User> updateUser(@PathVariable Integer id, @RequestBody User newUser) {
    Optional<User> userOpt = userRepository.findById(id);
    if (userOpt.isPresent()) {
      User user = userOpt.get();
      user.setName(newUser.getName());
      user.setEmail(newUser.getEmail());
      userRepository.save(user);
      return ResponseEntity.ok(user);
    }
    return ResponseEntity.notFound().build();
  }

  // DELETE /users/{id}: Delete a user
  @DeleteMapping("/{id}")
  public ResponseEntity<Void> deleteUser(@PathVariable Integer id) {
    if (userRepository.existsById(id)) {
      userRepository.deleteById(id);
      return ResponseEntity.ok().build();
    }
    return ResponseEntity.notFound().build();
  }
}
