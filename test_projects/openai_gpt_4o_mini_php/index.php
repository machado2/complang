<?php

// Set the response header to accept JSON
header('Content-Type: application/json');

// Connect to the PostgreSQL database
$host = 'host.docker.internal';
$port = '5432';
$dbname = 'complang';
$user = 'testuser';
$password = getenv('PGPASSWORD');

$dsn = "pgsql:host=$host;port=$port;dbname=$dbname";
try {
    $pdo = new PDO($dsn, $user, $password);
} catch (PDOException $e) {
    echo json_encode(['error' => $e->getMessage()]);
    exit();
}

// Handle the incoming request
$requestMethod = $_SERVER['REQUEST_METHOD'];
$uri = explode('/', trim($_SERVER['REQUEST_URI'], '/'));

// Route to the right function
if ($uri[0] === 'users') {
    switch ($requestMethod) {
        case 'POST':
            createUser($pdo);
            break;
        case 'GET':
            if (isset($uri[1])) {
                getUser($pdo, $uri[1]);
            } else {
                getUsers($pdo);
            }
            break;
        case 'PUT':
            updateUser($pdo, $uri[1]);
            break;
        case 'DELETE':
            deleteUser($pdo, $uri[1]);
            break;
        default:
            http_response_code(405);
            echo json_encode(['error' => 'Method Not Allowed']);
            break;
    }
} else {
    http_response_code(404);
    echo json_encode(['error' => 'Not Found']);
}

function createUser($pdo) {
    $data = json_decode(file_get_contents("php://input"), true);
    if (!isset($data['name']) || !isset($data['email'])) {
        http_response_code(400);
        echo json_encode(['error' => 'Bad Request']);
        return;
    }

    $stmt = $pdo->prepare("INSERT INTO users (name, email) VALUES (?, ?) RETURNING id");
    $stmt->execute([$data['name'], $data['email']]);
    $id = $stmt->fetchColumn();

    http_response_code(201);
    echo json_encode(['id' => $id, 'name' => $data['name'], 'email' => $data['email']]);
}

function getUsers($pdo) {
    $stmt = $pdo->query("SELECT * FROM users");
    $users = $stmt->fetchAll(PDO::FETCH_ASSOC);
    echo json_encode($users);
}

function getUser($pdo, $id) {
    $stmt = $pdo->prepare("SELECT * FROM users WHERE id = ?");
    $stmt->execute([$id]);
    $user = $stmt->fetch(PDO::FETCH_ASSOC);

    if ($user) {
        echo json_encode($user);
    } else {
        http_response_code(404);
        echo json_encode(['error' => 'User Not Found']);
    }
}

function updateUser($pdo, $id) {
    $data = json_decode(file_get_contents("php://input"), true);
    if (!isset($data['name']) || !isset($data['email'])) {
        http_response_code(400);
        echo json_encode(['error' => 'Bad Request']);
        return;
    }

    $stmt = $pdo->prepare("UPDATE users SET name = ?, email = ? WHERE id = ?");
    $success = $stmt->execute([$data['name'], $data['email'], $id]);

    if ($success) {
        http_response_code(204);
    } else {
        http_response_code(404);
        echo json_encode(['error' => 'User Not Found']);
    }
}

function deleteUser($pdo, $id) {
    $stmt = $pdo->prepare("DELETE FROM users WHERE id = ?");
    $success = $stmt->execute([$id]);

    if ($success) {
        http_response_code(204);
    } else {
        http_response_code(404);
        echo json_encode(['error' => 'User Not Found']);
    }
}
?>