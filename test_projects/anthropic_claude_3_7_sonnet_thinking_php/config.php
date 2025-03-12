<?php
// Database configuration
return [
    'host' => 'host.docker.internal',
    'port' => '5432',
    'database' => 'complang',
    'username' => 'testuser',
    'password' => getenv('PGPASSWORD'),
];
