<?php
// Database connection class
class Database {
    private $host = "host.docker.internal";
    private $db_name = "complang";
    private $username = "testuser";
    private $conn;

    // Get database connection
    public function getConnection() {
        $this->conn = null;
        try {
            $password = getenv("PGPASSWORD");
            $this->conn = new PDO(
                "pgsql:host=" . $this->host . ";dbname=" . $this->db_name,
                $this->username,
                $password
            );
            $this->conn->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
        } catch(PDOException $e) {
            echo "Connection error: " . $e->getMessage();
        }
        return $this->conn;
    }
}
?>