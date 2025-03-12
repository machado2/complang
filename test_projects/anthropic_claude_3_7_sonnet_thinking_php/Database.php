<?php
class Database {
    private $conn;
    private static $instance = null;
    
    private function __construct() {
        $config = require_once 'config.php';
        
        try {
            $dsn = "pgsql:host={$config['host']};port={$config['port']};dbname={$config['database']}";
            $this->conn = new PDO($dsn, $config['username'], $config['password'], [
                PDO::ATTR_ERRMODE => PDO::ERRMODE_EXCEPTION,
                PDO::ATTR_DEFAULT_FETCH_MODE => PDO::FETCH_ASSOC,
                PDO::ATTR_EMULATE_PREPARES => false
            ]);
        } catch (PDOException $e) {
            echo json_encode(['error' => 'Database connection failed: ' . $e->getMessage()]);
            exit;
        }
    }
    
    // Singleton pattern to ensure only one database connection
    public static function getInstance() {
        if (self::$instance === null) {
            self::$instance = new self();
        }
        return self::$instance;
    }
    
    // Get the database connection
    public function getConnection() {
        return $this->conn;
    }
}
