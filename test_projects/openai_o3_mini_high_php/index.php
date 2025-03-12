<?php
header('Content-Type: application/json');

// Establish database connection
try {
    $pgpassword = getenv('PGPASSWORD');
    if (!$pgpassword) {
        throw new Exception("PGPASSWORD environment variable not set");
    }
    $pdo = new PDO("pgsql:host=host.docker.internal;port=5432;dbname=complang", "testuser", $pgpassword);
    $pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
} catch (Exception $e) {
    http_response_code(500);
    echo json_encode(["error" => "Database connection error: " . $e->getMessage()]);
    exit;
}

// Determine request method and path
$method = $_SERVER['REQUEST_METHOD'];
$uri = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);
$segments = explode('/', trim($uri, '/'));

// Only routes starting with 'users' are supported
if ($segments[0] !== 'users') {
    http_response_code(404);
    echo json_encode(["error" => "Not Found"]);
    exit;
}

if (count($segments) === 1) {
    // Endpoint: /users
    if ($method === 'GET') {
        // List all users
        $stmt = $pdo->query("SELECT id, name, email FROM users");
        $users = $stmt->fetchAll(PDO::FETCH_ASSOC);
        echo json_encode($users);
    } elseif ($method === 'POST') {
        // Create a new user
        $data = json_decode(file_get_contents('php://input'), true);
        if (!isset($data['name']) || !isset($data['email'])) {
            http_response_code(400);
            echo json_encode(["error" => "Invalid input"]);
            exit;
        }
        $stmt = $pdo->prepare("INSERT INTO users (name, email) VALUES (:name, :email) RETURNING id");
        $stmt->execute([
            ':name' => $data['name'],
            ':email' => $data['email']
        ]);
        $newUser = $stmt->fetch(PDO::FETCH_ASSOC);
        if ($newUser) {
            $response = [
                "id" => $newUser['id'],
                "name" => $data['name'],
                "email" => $data['email']
            ];
            http_response_code(201);
            echo json_encode($response);
        } else {
            http_response_code(500);
            echo json_encode(["error" => "User creation failed"]);
        }
    } else {
        http_response_code(405);
        echo json_encode(["error" => "Method not allowed"]);
    }
} elseif (count($segments) === 2) {
    // Endpoint: /users/{id}
    $id = $segments[1];
    if (!ctype_digit($id)) {
        http_response_code(400);
        echo json_encode(["error" => "Invalid user id"]);
        exit;
    }
    if ($method === 'GET') {
        $stmt = $pdo->prepare("SELECT id, name, email FROM users WHERE id = :id");
        $stmt->execute([':id' => $id]);
        $user = $stmt->fetch(PDO::FETCH_ASSOC);
        if ($user) {
            echo json_encode($user);
        } else {
            http_response_code(404);
            echo json_encode(["error" => "User not found"]);
        }
    } elseif ($method === 'PUT') {
        $data = json_decode(file_get_contents('php://input'), true);
        if (!isset($data['name']) || !isset($data['email'])) {
            http_response_code(400);
            echo json_encode(["error" => "Invalid input"]);
            exit;
        }
        $stmt = $pdo->prepare("UPDATE users SET name = :name, email = :email WHERE id = :id");
        $stmt->execute([
            ':name' => $data['name'],
            ':email' => $data['email'],
            ':id' => $id
        ]);
        if ($stmt->rowCount() > 0) {
            // Return updated user
            $stmt = $pdo->prepare("SELECT id, name, email FROM users WHERE id = :id");
            $stmt->execute([':id' => $id]);
            $updatedUser = $stmt->fetch(PDO::FETCH_ASSOC);
            http_response_code(200);
            echo json_encode($updatedUser);
        } else {
            http_response_code(404);
            echo json_encode(["error" => "User not found or no changes made"]);
        }
    } elseif ($method === 'DELETE') {
        $stmt = $pdo->prepare("DELETE FROM users WHERE id = :id");
        $stmt->execute([':id' => $id]);
        if ($stmt->rowCount() > 0) {
            http_response_code(204);
        } else {
            http_response_code(404);
            echo json_encode(["error" => "User not found"]);
        }
    } else {
        http_response_code(405);
        echo json_encode(["error" => "Method not allowed"]);
    }
} else {
    http_response_code(404);
    echo json_encode(["error" => "Not Found"]);
}
?>
