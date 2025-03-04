<?php

require_once 'vendor/autoload.php';

use Dotenv\Dotenv;

$dotenv = Dotenv::createImmutable(__DIR__);
$dotenv->load();

$host = 'host.docker.internal';
$port = 5432;
$dbname = 'test_google_gemini_2_0_flash_001_php';
$user = 'postgres';
$password = $_ENV['PGPASSWORD'];

try {
    $pdo = new PDO("pgsql:host=$host;port=$port;dbname=$dbname;user=$user;password=$password");
    $pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);

    // Check if the users table exists; create it if not
    $result = $pdo->query("SELECT to_regclass('users')");
    if (!$result->fetchColumn()) {
        $pdo->exec("
            CREATE TABLE users (
                id SERIAL PRIMARY KEY,
                name TEXT NOT NULL,
                email TEXT NOT NULL
            )
        ");
    }

} catch (PDOException $e) {
    http_response_code(500);
    echo json_encode(['error' => 'Database connection failed: ' . $e->getMessage()]);
    exit;
}


header('Content-Type: application/json');

$request_method = $_SERVER['REQUEST_METHOD'];
$request_uri = $_SERVER['REQUEST_URI'];

switch (true) {
    case preg_match('/^\/users$/', $request_uri) && $request_method == 'POST':
        // Create User
        createUser($pdo);
        break;
    case preg_match('/^\/users$/', $request_uri) && $request_method == 'GET':
        // Get All Users
        getAllUsers($pdo);
        break;
    case preg_match('/^\/users\/(\d+)$/', $request_uri, $matches) && $request_method == 'GET':
        // Get User
        getUser($pdo, $matches[1]);
        break;
    case preg_match('/^\/users\/(\d+)$/', $request_uri, $matches) && $request_method == 'PUT':
        // Update User
        updateUser($pdo, $matches[1]);
        break;
    case preg_match('/^\/users\/(\d+)$/', $request_uri, $matches) && $request_method == 'DELETE':
        // Delete User
        deleteUser($pdo, $matches[1]);
        break;
    default:
        http_response_code(404);
        echo json_encode(['error' => 'Not Found']);
        break;
}

function createUser($pdo) {
    $data = json_decode(file_get_contents('php://input'), true);

    if (!isset($data['name']) || !isset($data['email'])) {
       http_response_code(400);
       echo json_encode(['error' => 'Missing required parameters']);
       return;
    }

    $name = $data['name'];
    $email = $data['email'];

    try {
        $stmt = $pdo->prepare("INSERT INTO users (name, email) VALUES (:name, :email)");
        $stmt->bindParam(':name', $name);
        $stmt->bindParam(':email', $email);
        $stmt->execute();

        $id = $pdo->lastInsertId();

        $user = ['id' => (int)$id, 'name' => $name, 'email' => $email];
        http_response_code(201);
        echo json_encode($user);

    } catch (PDOException $e) {
        http_response_code(500);
        echo json_encode(['error' => 'Failed to create user: ' . $e->getMessage()]);
    }
}

function getAllUsers($pdo) {
    try {
        $stmt = $pdo->query("SELECT * FROM users");
        $users = [];
        while ($row = $stmt->fetch(PDO::FETCH_ASSOC)) {
            $users[] = $row;
        }

        echo json_encode($users);

    } catch (PDOException $e) {
        http_response_code(500);
        echo json_encode(['error' => 'Failed to get users: ' . $e->getMessage()]);
    }
}

function getUser($pdo, $id) {
    try {
        $stmt = $pdo->prepare("SELECT * FROM users WHERE id = :id");
        $stmt->bindParam(':id', $id);
        $stmt->execute();

        $user = $stmt->fetch(PDO::FETCH_ASSOC);

        if ($user) {
            echo json_encode($user);
        } else {
            http_response_code(404);
            echo json_encode(['error' => 'User not found']);
        }

    } catch (PDOException $e) {
        http_response_code(500);
        echo json_encode(['error' => 'Failed to get user: ' . $e->getMessage()]);
    }
}

function updateUser($pdo, $id) {
    $data = json_decode(file_get_contents('php://input'), true);

    if (!isset($data['name']) || !isset($data['email'])) {
       http_response_code(400);
       echo json_encode(['error' => 'Missing required parameters']);
       return;
    }

    $name = $data['name'];
    $email = $data['email'];

    try {
        $stmt = $pdo->prepare("UPDATE users SET name = :name, email = :email WHERE id = :id");
        $stmt->bindParam(':id', $id);
        $stmt->bindParam(':name', $name);
        $stmt->bindParam(':email', $email);
        $stmt->execute();

        if ($stmt->rowCount() > 0) {
            http_response_code(200); // Or 204 No Content
        } else {
            http_response_code(404);
            echo json_encode(['error' => 'User not found']);
        }

    } catch (PDOException $e) {
        http_response_code(500);
        echo json_encode(['error' => 'Failed to update user: ' . $e->getMessage()]);
    }
}

function deleteUser($pdo, $id) {
    try {
        $stmt = $pdo->prepare("DELETE FROM users WHERE id = :id");
        $stmt->bindParam(':id', $id);
        $stmt->execute();

        if ($stmt->rowCount() > 0) {
            http_response_code(200); // Or 204 No Content
        } else {
            http_response_code(404);
            echo json_encode(['error' => 'User not found']);
        }

    } catch (PDOException $e) {
        http_response_code(500);
        echo json_encode(['error' => 'Failed to delete user: ' . $e->getMessage()]);
    }
}
?>
