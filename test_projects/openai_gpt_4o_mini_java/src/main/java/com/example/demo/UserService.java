package com.example.demo;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
public class UserService {

    private List<User> users = new ArrayList<>();
    private long nextId = 1;

    public User createUser(User user) {
        user.setId(nextId++);
        users.add(user);
        return user;
    }

    public List<User> getAllUsers() {
        return users;
    }

    public User getUserById(long id) {
        return users.stream().filter(user -> user.getId() == id).findFirst().orElse(null);
    }

    public boolean updateUser(long id, User user) {
        Optional<User> existingUserOpt = users.stream().filter(u -> u.getId() == id).findFirst();
        if (existingUserOpt.isPresent()) {
            User existingUser = existingUserOpt.get();
            existingUser.setName(user.getName());
            existingUser.setEmail(user.getEmail());
            return true;
        }
        return false;
    }

    public boolean deleteUser(long id) {
        return users.removeIf(user -> user.getId() == id);
    }
}