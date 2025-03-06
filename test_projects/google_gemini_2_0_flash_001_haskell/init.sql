-- Create the database
CREATE DATABASE complang;

-- Create the user with a simple password
CREATE USER testuser WITH PASSWORD 'Saloon5-Moody-Observing';

-- Grant CONNECT privilege on the database to the user
GRANT CONNECT ON DATABASE complang TO testuser;

-- Connect to the newly created database
\connect complang

-- Create the users table with the specified structure
CREATE TABLE IF NOT EXISTS users (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    email TEXT NOT NULL
);

-- Grant CRUD permissions (SELECT, INSERT, UPDATE, DELETE) on the users table to the user
GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE users TO testuser;
GRANT USAGE, SELECT ON SEQUENCE users_id_seq TO testuser;
