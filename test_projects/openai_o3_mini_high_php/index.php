<?php
error_reporting(E_ALL);
ini_set('display_errors', 1);

// Set JSON header for all responses
header('Content-Type: application/json');

// Connect to the PostgreSQL database using environment password
$dsn = "pgsql:host=host.docker.internal;port=5432;dbname=complang";
$dbuser = "testuser";
$dbpass = getenv("PGPASSWORD");

try {
    $pdo = new PDO($dsn, $dbuser, $dbpass, [
        PDO::ATTR_ERRMODE => PDO::ERRMODE_EXCEPTION
    ]);
} catch (PDOException $e) {
    http_response_code(500);
    echo json_encode(['error' => 'Database connection failed']);
    exit;
}

// Determine the HTTP method and requested URI segments
$method = $_SERVER['REQUEST_METHOD'];
$uri = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);
$segments = explode('/', trim($uri, '/'));

// Only routes under "users" are handled
if (empty($segments[0]) || $segments[0] !== "users") {
    http_response_code(404);
    echo json_encode(['error' => 'Not Found']);
    exit;
}

// Route: /users without an ID
if (count($segments) === 1) {
    if ($method === 'GET') {
        // List all users
        $stmt = $pdo->query("SELECT id, name, email FROM users");
        $users = $stmt->fetchAll(PDO::FETCH_ASSOC);
        echo json_encode($users);
        exit;
    } elseif ($method === 'POST') {
        // Create a new user
        $input = file_get_contents("php://input");
        $data = json_decode($input, true);
        if (!isset($data['name']) || !isset($data['email'])) {
            http_response_code(400);
            echo json_encode(['error' => 'Invalid input']);
            exit;
        }
        try {
            $stmt = $pdo->prepare("INSERT INTO users (name, email) VALUES (:name, :email) RETURNING id");
            $stmt->execute([':name' => $data['name'], ':email' => $data['email']]);
            $newUser = $stmt->fetch(PDO::FETCH_ASSOC);
            if ($newUser) {
                http_response_code(201);
                echo json_encode([
                    'id' => (int)$newUser['id'],
                    'name' => $data['name'],
                    'email' => $data['email']
                ]);
            } else {
                http_response_code(500);
                echo json_encode(['error' => 'Failed to create user']);
            }
        } catch (PDOException $e) {
            http_response_code(500);
            echo json_encode(['error' => 'Database error: ' . $e->getMessage()]);
        }
        exit;
    } else {
        http_response_code(405);
        echo json_encode(['error' => 'Method not allowed']);
        exit;
    }
}

// Route: /users/{id}
elseif (count($segments) === 2) {
    $id = $segments[1];
    if (!is_numeric($id)) {
        http_response_code(400);
        echo json_encode(['error' => 'Invalid user id']);
        exit;
    }
    $id = (int)$id;

    if ($method === 'GET') {
        // Retrieve a user by id
        $stmt = $pdo->prepare("SELECT id, name, email FROM users WHERE id = ?");
        $stmt->execute([$id]);
        $user = $stmt->fetch(PDO::FETCH_ASSOC);
        if ($user) {
            echo json_encode($user);
        } else {
            http_response_code(404);
            echo json_encode(['error' => 'User not found']);
        }
        exit;
    } elseif ($method === 'PUT') {
        // Update a user by id
        $input = file_get_contents("php://input");
        $data = json_decode($input, true);
        if (!isset($data['name']) || !isset($data['email'])) {
            http_response_code(400);
            echo json_encode(['error' => 'Invalid input']);
            exit;
        }
        try {
            $stmt = $pdo->prepare("UPDATE users SET name = :name, email = :email WHERE id = :id");
            $stmt->execute([
                ':name' => $data['name'],
                ':email' => $data['email'],
                ':id' => $id
            ]);
            if ($stmt->rowCount() === 0) {
                http_response_code(404);
                echo json_encode(['error' => 'User not found']);
            } else {
                // Return the updated user
                $stmt = $pdo->prepare("SELECT id, name, email FROM users WHERE id = ?");
                $stmt->execute([$id]);
                $updatedUser = $stmt->fetch(PDO::FETCH_ASSOC);
                echo json_encode($updatedUser);
            }
        } catch (PDOException $e) {
            http_response_code(500);
            echo json_encode(['error' => 'Database error: ' . $e->getMessage()]);
        }
        exit;
    } elseif ($method === 'DELETE') {
        // Delete a user by id
        try {
            $stmt = $pdo->prepare("DELETE FROM users WHERE id = ?");
            $stmt->execute([$id]);
            if ($stmt->rowCount() === 0) {
                http_response_code(404);
                echo json_encode(['error' => 'User not found']);
            } else {
                http_response_code(204);
            }
        } catch (PDOException $e) {
            http_response_code(500);
            echo json_encode(['error' => 'Database error: ' . $e->getMessage()]);
        }
        exit;
    } else {
        http_response_code(405);
        echo json_encode(['error' => 'Method not allowed']);
        exit;
    }
} else {
    http_response_code(404);
    echo json_encode(['error' => 'Not Found']);
    exit;
}
?>
