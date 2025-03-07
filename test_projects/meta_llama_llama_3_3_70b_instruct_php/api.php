
<?php
// Configuration
$host = 'host.docker.internal';
$port = 5432;
$dbname = 'complang';
$user = 'testuser';
$password = getenv('PGPASSWORD');

// Connect to the database
$conn = pg_connect("host=$host port=$port dbname=$dbname user=$user password=$password");

// Check connection
if (!$conn) {
    die("Connection failed: ". pg_last_error());
}

// Define API endpoints
function create_user($data) {
    global $conn;
    $name = $data['name'];
    $email = $data['email'];
    $query = "INSERT INTO users (name, email) VALUES ('$name', '$email') RETURNING *";
    $result = pg_query($conn, $query);
    $user = pg_fetch_assoc($result);
    return $user;
}

function get_users() {
    global $conn;
    $query = "SELECT * FROM users";
    $result = pg_query($conn, $query);
    $users = array();
    while ($row = pg_fetch_assoc($result)) {
        $users[] = $row;
    }
    return $users;
}

function get_user($id) {
    global $conn;
    $query = "SELECT * FROM users WHERE id = $id";
    $result = pg_query($conn, $query);
    $user = pg_fetch_assoc($result);
    return $user;
}

function update_user($id, $data) {
    global $conn;
    $name = $data['name'];
    $email = $data['email'];
    $query = "UPDATE users SET name = '$name', email = '$email' WHERE id = $id RETURNING *";
    $result = pg_query($conn, $query);
    $user = pg_fetch_assoc($result);
    return $user;
}

function delete_user($id) {
    global $conn;
    $query = "DELETE FROM users WHERE id = $id";
    $result = pg_query($conn, $query);
    return $result;
}

// Handle API requests
if ($_SERVER['REQUEST_METHOD'] == 'POST' && $_SERVER['PATH_INFO'] == '/users') {
    $data = json_decode(file_get_contents('php://input'), true);
    $user = create_user($data);
    header('Content-Type: application/json');
    header('HTTP/1.1 201 Created');
    echo json_encode($user);
} elseif ($_SERVER['REQUEST_METHOD'] == 'GET' && $_SERVER['PATH_INFO'] == '/users') {
    $users = get_users();
    header('Content-Type: application/json');
    header('HTTP/1.1 200 OK');
    echo json_encode($users);
} elseif ($_SERVER['REQUEST_METHOD'] == 'GET' && preg_match('/\/users\/(\d+)/', $_SERVER['PATH_INFO'], $matches)) {
    $id = $matches[1];
    $user = get_user($id);
    if ($user) {
        header('Content-Type: application/json');
        header('HTTP/1.1 200 OK');
        echo json_encode($user);
    } else {
        header('HTTP/1.1 404 Not Found');
    }
} elseif ($_SERVER['REQUEST_METHOD'] == 'PUT' && preg_match('/\/users\/(\d+)/', $_SERVER['PATH_INFO'], $matches)) {
    $id = $matches[1];
    $data = json_decode(file_get_contents('php://input'), true);
    $user = update_user($id, $data);
    if ($user) {
        header('Content-Type: application/json');
        header('HTTP/1.1 200 OK');
        echo json_encode($user);
    } else {
        header('HTTP/1.1 404 Not Found');
    }
} elseif ($_SERVER['REQUEST_METHOD'] == 'DELETE' && preg_match('/\/users\/(\d+)/', $_SERVER['PATH_INFO'], $matches)) {
    $id = $matches[1];
    $result = delete_user($id);
    if ($result) {
        header('HTTP/1.1 204 No Content');
    } else {
        header('HTTP/1.1 404 Not Found');
    }
} else {
    header('HTTP/1.1 405 Method Not Allowed');
}
?>
