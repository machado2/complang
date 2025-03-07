package com.example.usersapi.service;

import com.example.usersapi.dto.UserDTO;
import com.example.usersapi.dto.UserResponseDTO;
import com.example.usersapi.model.User;
import com.example.usersapi.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class UserService {

    private final UserRepository userRepository;

    @Autowired
    public UserService(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    public UserResponseDTO createUser(UserDTO userDTO) {
        User user = new User(userDTO.getName(), userDTO.getEmail());
        User savedUser = userRepository.save(user);
        return convertToResponseDTO(savedUser);
    }

    public List<UserResponseDTO> getAllUsers() {
        return userRepository.findAll()
                .stream()
                .map(this::convertToResponseDTO)
                .collect(Collectors.toList());
    }

    public Optional<UserResponseDTO> getUserById(Integer id) {
        return userRepository.findById(id)
                .map(this::convertToResponseDTO);
    }

    public Optional<UserResponseDTO> updateUser(Integer id, UserDTO userDTO) {
        return userRepository.findById(id)
                .map(user -> {
                    user.setName(userDTO.getName());
                    user.setEmail(userDTO.getEmail());
                    return userRepository.save(user);
                })
                .map(this::convertToResponseDTO);
    }

    public boolean deleteUser(Integer id) {
        if (userRepository.existsById(id)) {
            userRepository.deleteById(id);
            return true;
        }
        return false;
    }

    private UserResponseDTO convertToResponseDTO(User user) {
        return new UserResponseDTO(user.getId(), user.getName(), user.getEmail());
    }
}
