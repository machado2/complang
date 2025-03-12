<?php
require_once 'Database.php';

class User {
    private $conn;
    
    public function __construct() {
        $database = Database::getInstance();
        $this->conn = $database->getConnection();
    }
    
    // Create a new user
    public function create($data) {
        // Validate required fields
        if (!isset($data['name']) || !isset($data['email'])) {
            return ['error' => 'Name and email are required fields', 'status' => 400];
        }
        
        $query = "INSERT INTO users (name, email) VALUES (:name, :email) RETURNING id, name, email";
        
        try {
            $stmt = $this->conn->prepare($query);
            $stmt->bindParam(':name', $data['name']);
            $stmt->bindParam(':email', $data['email']);
            $stmt->execute();
            
            return [
                'data' => $stmt->fetch(),
                'status' => 201
            ];
        } catch (PDOException $e) {
            return ['error' => 'Error creating user: ' . $e->getMessage(), 'status' => 500];
        }
    }
    
    // Get all users
    public function getAll() {
        $query = "SELECT id, name, email FROM users";
        
        try {
            $stmt = $this->conn->prepare($query);
            $stmt->execute();
            
            return [
                'data' => $stmt->fetchAll(),
                'status' => 200
            ];
        } catch (PDOException $e) {
            return ['error' => 'Error retrieving users: ' . $e->getMessage(), 'status' => 500];
        }
    }
    
    // Get a single user by ID
    public function getById($id) {
        $query = "SELECT id, name, email FROM users WHERE id = :id";
        
        try {
            $stmt = $this->conn->prepare($query);
            $stmt->bindParam(':id', $id);
            $stmt->execute();
            
            $user = $stmt->fetch();
            
            if (!$user) {
                return ['error' => 'User not found', 'status' => 404];
            }
            
            return [
                'data' => $user,
                'status' => 200
            ];
        } catch (PDOException $e) {
            return ['error' => 'Error retrieving user: ' . $e->getMessage(), 'status' => 500];
        }
    }
    
    // Update a user
    public function update($id, $data) {
        // Check if user exists
        $query = "SELECT id FROM users WHERE id = :id";
        $stmt = $this->conn->prepare($query);
        $stmt->bindParam(':id', $id);
        $stmt->execute();
        
        if (!$stmt->fetch()) {
            return ['error' => 'User not found', 'status' => 404];
        }
        
        // Validate required fields
        if (!isset($data['name']) || !isset($data['email'])) {
            return ['error' => 'Name and email are required fields', 'status' => 400];
        }
        
        $query = "UPDATE users SET name = :name, email = :email WHERE id = :id";
        
        try {
            $stmt = $this->conn->prepare($query);
            $stmt->bindParam(':name', $data['name']);
            $stmt->bindParam(':email', $data['email']);
            $stmt->bindParam(':id', $id);
            $stmt->execute();
            
            return [
                'message' => 'User updated successfully',
                'status' => 200
            ];
        } catch (PDOException $e) {
            return ['error' => 'Error updating user: ' . $e->getMessage(), 'status' => 500];
        }
    }
    
    // Delete a user
    public function delete($id) {
        // Check if user exists
        $query = "SELECT id FROM users WHERE id = :id";
        $stmt = $this->conn->prepare($query);
        $stmt->bindParam(':id', $id);
        $stmt->execute();
        
        if (!$stmt->fetch()) {
            return ['error' => 'User not found', 'status' => 404];
        }
        
        $query = "DELETE FROM users WHERE id = :id";
        
        try {
            $stmt = $this->conn->prepare($query);
            $stmt->bindParam(':id', $id);
            $stmt->execute();
            
            return [
                'message' => 'User deleted successfully',
                'status' => 200
            ];
        } catch (PDOException $e) {
            return ['error' => 'Error deleting user: ' . $e->getMessage(), 'status' => 500];
        }
    }
}
