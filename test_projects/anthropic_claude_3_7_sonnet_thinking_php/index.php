<?php
header("Content-Type: application/json");

// Allow cross-origin requests
header("Access-Control-Allow-Origin: *");
header("Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS");
header("Access-Control-Allow-Headers: Content-Type, Access-Control-Allow-Headers, Authorization, X-Requested-With");

// Handle preflight requests
if ($_SERVER['REQUEST_METHOD'] === 'OPTIONS') {
    http_response_code(200);
    exit;
}

// Include database connection
require_once 'db.php';

// Initialize database connection
$database = new Database();
$db = $database->getConnection();

// Get request URI and method
$request_uri = $_SERVER['REQUEST_URI'];
$request_method = $_SERVER['REQUEST_METHOD'];

// Parse request URI
$uri_parts = explode('/', trim($request_uri, '/'));

// API endpoints
$endpoint = isset($uri_parts[0]) ? $uri_parts[0] : '';
$id = isset($uri_parts[1]) ? $uri_parts[1] : null;

// Handle only /users endpoint
if ($endpoint !== 'users') {
    http_response_code(404);
    echo json_encode(["message" => "Endpoint not found"]);
    exit;
}

// Get request body for POST and PUT requests
$data = null;
if ($request_method === 'POST' || $request_method === 'PUT') {
    $data = json_decode(file_get_contents("php://input"), true);
}

// CRUD operations for users
switch ($request_method) {
    case 'GET':
        if ($id) {
            // GET /users/{id} - Retrieve a specific user
            $stmt = $db->prepare("SELECT * FROM users WHERE id = :id");
            $stmt->bindParam(':id', $id);
            $stmt->execute();
            $user = $stmt->fetch(PDO::FETCH_ASSOC);
            
            if ($user) {
                http_response_code(200);
                echo json_encode($user);
            } else {
                http_response_code(404);
                echo json_encode(["message" => "User not found"]);
            }
        } else {
            // GET /users - Retrieve all users
            $stmt = $db->prepare("SELECT * FROM users");
            $stmt->execute();
            $users = $stmt->fetchAll(PDO::FETCH_ASSOC);
            
            http_response_code(200);
            echo json_encode($users);
        }
        break;
        
    case 'POST':
        // POST /users - Create a new user
        if (!$id && $data) {
            // Validate required fields
            if (isset($data['name']) && isset($data['email'])) {
                $stmt = $db->prepare("INSERT INTO users (name, email) VALUES (:name, :email)");
                $stmt->bindParam(':name', $data['name']);
                $stmt->bindParam(':email', $data['email']);
                
                if ($stmt->execute()) {
                    $user_id = $db->lastInsertId();
                    $user = [
                        "id" => (int)$user_id,
                        "name" => $data['name'],
                        "email" => $data['email']
                    ];
                    
                    http_response_code(201);
                    echo json_encode($user);
                } else {
                    http_response_code(500);
                    echo json_encode(["message" => "Failed to create user"]);
                }
            } else {
                http_response_code(400);
                echo json_encode(["message" => "Missing required fields: name, email"]);
            }
        } else {
            http_response_code(400);
            echo json_encode(["message" => "Invalid request"]);
        }
        break;
        
    case 'PUT':
        // PUT /users/{id} - Update a user
        if ($id && $data) {
            // Check if user exists
            $check_stmt = $db->prepare("SELECT COUNT(*) FROM users WHERE id = :id");
            $check_stmt->bindParam(':id', $id);
            $check_stmt->execute();
            
            if ($check_stmt->fetchColumn()) {
                // Validate required fields
                if (isset($data['name']) && isset($data['email'])) {
                    $stmt = $db->prepare("UPDATE users SET name = :name, email = :email WHERE id = :id");
                    $stmt->bindParam(':name', $data['name']);
                    $stmt->bindParam(':email', $data['email']);
                    $stmt->bindParam(':id', $id);
                    
                    if ($stmt->execute()) {
                        http_response_code(200);
                        echo json_encode(["message" => "User updated successfully"]);
                    } else {
                        http_response_code(500);
                        echo json_encode(["message" => "Failed to update user"]);
                    }
                } else {
                    http_response_code(400);
                    echo json_encode(["message" => "Missing required fields: name, email"]);
                }
            } else {
                http_response_code(404);
                echo json_encode(["message" => "User not found"]);
            }
        } else {
            http_response_code(400);
            echo json_encode(["message" => "Invalid request"]);
        }
        break;
        
    case 'DELETE':
        // DELETE /users/{id} - Delete a user
        if ($id) {
            // Check if user exists
            $check_stmt = $db->prepare("SELECT COUNT(*) FROM users WHERE id = :id");
            $check_stmt->bindParam(':id', $id);
            $check_stmt->execute();
            
            if ($check_stmt->fetchColumn()) {
                $stmt = $db->prepare("DELETE FROM users WHERE id = :id");
                $stmt->bindParam(':id', $id);
                
                if ($stmt->execute()) {
                    http_response_code(200);
                    echo json_encode(["message" => "User deleted successfully"]);
                } else {
                    http_response_code(500);
                    echo json_encode(["message" => "Failed to delete user"]);
                }
            } else {
                http_response_code(404);
                echo json_encode(["message" => "User not found"]);
            }
        } else {
            http_response_code(400);
            echo json_encode(["message" => "Invalid request"]);
        }
        break;
        
    default:
        http_response_code(405);
        echo json_encode(["message" => "Method not allowed"]);
}
?>