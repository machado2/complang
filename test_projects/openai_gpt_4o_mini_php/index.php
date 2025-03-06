<?php
// Enable error reporting for debugging
error_reporting(E_ALL);
ini_set('display_errors', 1);

// Database connection
$dbconn = pg_connect("host=host.docker.internal port=5432 dbname=complang user=testuser password='Saloon5-Moody-Observing'");

if (!$dbconn) {
    die("Error in connection: " . pg_last_error());
}

// Set headers for JSON responses
header('Content-Type: application/json');

// Handle HTTP requests
$requestMethod = $_SERVER['REQUEST_METHOD'];
$uri = explode('/', trim($_SERVER['PATH_INFO'],'/'));

// Routing
if ($uri[0] === 'users') {
    switch ($requestMethod) {
        case 'POST':
            createUser();
            break;
        case 'GET':
            if (isset($uri[1])) {
                getUser($uri[1]);
            } else {
                getUsers();
            }
            break;
        case 'PUT':
            updateUser($uri[1]);
            break;
        case 'DELETE':
            deleteUser($uri[1]);
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

// Functions for CRUD operations
function createUser() {
    global $dbconn;
    $data = json_decode(file_get_contents('php://input'), true);
    $name = $data['name'];
    $email = $data['email'];
    $result = pg_query_params($dbconn, "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id;", [$name, $email]);
    if ($result) {
        $id = pg_fetch_result($result, 0, 'id');
        http_response_code(201);
        echo json_encode(['id' => $id, 'name' => $name, 'email' => $email]);
    } else {
        http_response_code(400);
        echo json_encode(['error' => 'Could not create user']);
    }
}

function getUsers() {
    global $dbconn;
    $result = pg_query($dbconn, "SELECT id, name, email FROM users;");
    $users = pg_fetch_all($result);
    http_response_code(200);
    echo json_encode($users);
}

function getUser($id) {
    global $dbconn;
    $result = pg_query_params($dbconn, "SELECT id, name, email FROM users WHERE id = $1;", [$id]);
    if ($result && pg_num_rows($result) > 0) {
        $user = pg_fetch_assoc($result);
        http_response_code(200);
        echo json_encode($user);
    } else {
        http_response_code(404);
        echo json_encode(['error' => 'User not found']);
    }
}

function updateUser($id) {
    global $dbconn;
    $data = json_decode(file_get_contents('php://input'), true);
    $name = $data['name'];
    $email = $data['email'];
    $result = pg_query_params($dbconn, "UPDATE users SET name = $1, email = $2 WHERE id = $3;", [$name, $email, $id]);
    if ($result && pg_affected_rows($result) > 0) {
        http_response_code(204);
    } else {
        http_response_code(404);
        echo json_encode(['error' => 'User not found']);
    }
}

function deleteUser($id) {
    global $dbconn;
    $result = pg_query_params($dbconn, "DELETE FROM users WHERE id = $1;", [$id]);
    if ($result && pg_affected_rows($result) > 0) {
        http_response_code(204);
    } else {
        http_response_code(404);
        echo json_encode(['error' => 'User not found']);
    }
}
?>