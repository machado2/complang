<?php
require __DIR__ . '/vendor/autoload.php';

use Dotenv\Dotenv;

$dotenv = Dotenv::createImmutable(__DIR__);
$dotenv->load();

$host = 'host.docker.internal';
$port = 5432;
$dbname = 'complang';
$user = 'testuser';
$password = $_ENV['PGPASSWORD'];

$dsn = "pgsql:host=$host;port=$port;dbname=$dbname;user=$user;password=$password";

try {
    $pdo = new PDO($dsn);
    $pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
} catch (PDOException $e) {
    http_response_code(500);
    echo json_encode(['error' => 'Database connection failed: ' . $e->getMessage()]);
    exit;
}

// CORS Headers
header("Access-Control-Allow-Origin: *");
header("Access-Control-Allow-Methods: POST, GET, PUT, DELETE, OPTIONS");
header("Access-Control-Allow-Headers: Content-Type");

if ($_SERVER['REQUEST_METHOD'] == 'OPTIONS') {
    http_response_code(200);
    exit;
}

$request_uri = $_SERVER['REQUEST_URI'];
$method = $_SERVER['REQUEST_METHOD'];

if (strpos($request_uri, '/users') === 0) {
    $uri_segments = explode('/', trim($request_uri, '/'));
    $id = isset($uri_segments[2]) ? (int)$uri_segments[2] : null;

    switch ($method) {
        case 'POST':
            createUser($pdo);
            break;
        case 'GET':
            if ($id) {
                getUser($pdo, $id);
            } else {
                getAllUsers($pdo);
            }
            break;
        case 'PUT':
            if ($id) {
                updateUser($pdo, $id);
            } else {
                http_response_code(400);
                echo json_encode(['error' => 'User ID required for update']);
            }
            break;
        case 'DELETE':
            if ($id) {
                deleteUser($pdo, $id);
            } else {
                http_response_code(400);
                echo json_encode(['error' => 'User ID required for delete']);
            }
            break;
        default:
            http_response_code(405);
            echo json_encode(['error' => 'Method not allowed']);
    }
} else {
    http_response_code(404);
    echo json_encode(['error' => 'Endpoint not found']);
}

function createUser($pdo) {
    $body = json_decode(file_get_contents('php://input'), true);

    if (!isset($body['name']) || !isset($body['email'])) {
        http_response_code(400);
        echo json_encode(['error' => 'Name and email are required']);
        return;
    }

    $name = $body['name'];
    $email = $body['email'];

    try {
        $stmt = $pdo->prepare("INSERT INTO users (name, email) VALUES (:name, :email) RETURNING id");
        $stmt->execute(['name' => $name, 'email' => $email]);
        $user_id = $stmt->fetchColumn(); // Get the inserted ID

        http_response_code(201);
        echo json_encode(['id' => (int)$user_id, 'name' => $name, 'email' => $email]);
    } catch (PDOException $e) {
        http_response_code(500);
        echo json_encode(['error' => 'Failed to create user: ' . $e->getMessage()]);
    }
}


function getAllUsers($pdo) {
    try {
        $stmt = $pdo->query("SELECT * FROM users");
        $users = $stmt->fetchAll(PDO::FETCH_ASSOC);
        echo json_encode($users);
    } catch (PDOException $e) {
        http_response_code(500);
        echo json_encode(['error' => 'Failed to get users: ' . $e->getMessage()]);
    }
}

function getUser($pdo, $id) {
    try {
        $stmt = $pdo->prepare("SELECT * FROM users WHERE id = :id");
        $stmt->execute(['id' => $id]);
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
    $body = json_decode(file_get_contents('php://input'), true);

    if (!isset($body['name']) || !isset($body['email'])) {
        http_response_code(400);
        echo json_encode(['error' => 'Name and email are required']);
        return;
    }

    $name = $body['name'];
    $email = $body['email'];

    try {
        $stmt = $pdo->prepare("UPDATE users SET name = :name, email = :email WHERE id = :id");
        $stmt->execute(['id' => $id, 'name' => $name, 'email' => $email]);

        if ($stmt->rowCount() > 0) {
            http_response_code(204); // Or 200 with updated data
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
        $stmt->execute(['id' => $id]);

        if ($stmt->rowCount() > 0) {
            http_response_code(204); // No Content indicating success
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
