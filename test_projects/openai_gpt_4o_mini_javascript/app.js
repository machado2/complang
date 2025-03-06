const express = require('express');
const bodyParser = require('body-parser');
const { Pool } = require('pg');
const app = express();
const port = 8080;

// Middleware
app.use(bodyParser.json());

// PostgreSQL connection setup for the default database
const pool = new Pool({
  user: 'postgres',
  host: 'host.docker.internal',
  database: 'postgres', // Connect to the default database initially
  password: process.env.PGPASSWORD,
  port: 5432,
});

// Global variable for user database pool
let userPool;

// Function to create the database if it doesn't exist
const createDatabase = async () => {
  const client = await pool.connect();
  try {
    console.log('Dropping existing database if it exists...');
    await client.query("DROP DATABASE IF EXISTS \"test_openai_gpt_4o_mini_javascript\"");
    console.log('Creating database: test_openai_gpt_4o_mini_javascript');
    await client.query('CREATE DATABASE "test_openai_gpt_4o_mini_javascript" WITH OWNER postgres;');
  } catch (error) {
    console.error('Error creating database:', error);
  } finally {
    client.release();
  }
};

// Function to set up the connection to the created database and create the user table
const setupDatabaseAndTable = async () => {
  console.log('Setting up user database...');
  userPool = new Pool({
    user: 'postgres',
    host: 'host.docker.internal',
    database: 'test_openai_gpt_4o_mini_javascript', // Use the created database
    password: process.env.PGPASSWORD,
    port: 5432,
  });

  try {
    console.log('Creating users table...');
    const query = `
      CREATE TABLE IF NOT EXISTS users (
        id SERIAL PRIMARY KEY,
        name TEXT,
        email TEXT
      )
    `;
    await userPool.query(query);
    console.log('Users table created or already exists.');
  } catch (error) {
    console.error('Error setting up the users table:', error);
  }
};

// Start the setup process
createDatabase()
  .then(setupDatabaseAndTable)
  .then(() => {
    app.listen(port, () => {
      console.log(`Server running at http://localhost:${port}`);
    });
  })
  .catch(e => {
    console.error('Error during the database setup:', e);
  });

// CRUD operations
app.post('/users', async (req, res) => {
  const { name, email } = req.body;
  try {
    const result = await userPool.query('INSERT INTO users (name, email) VALUES ($1, $2) RETURNING *', [name, email]);
    res.status(201).json(result.rows[0]);
  } catch (e) {
    console.error('Error inserting user:', e);
    res.status(500).send('Internal Server Error');
  }
});

app.get('/users', async (req, res) => {
  try {
    const result = await userPool.query('SELECT * FROM users');
    res.status(200).json(result.rows);
  } catch (e) {
    console.error('Error retrieving users:', e);
    res.status(500).send('Internal Server Error');
  }
});

app.get('/users/:id', async (req, res) => {
  const { id } = req.params;
  try {
    const result = await userPool.query('SELECT * FROM users WHERE id = $1', [id]);
    if (result.rows.length === 0) {
      return res.status(404).send('User not found');
    }
    res.status(200).json(result.rows[0]);
  } catch (e) {
    console.error('Error retrieving user:', e);
    res.status(500).send('Internal Server Error');
  }
});

app.put('/users/:id', async (req, res) => {
  const { id } = req.params;
  const { name, email } = req.body;
  try {
    const result = await userPool.query('UPDATE users SET name = $1, email = $2 WHERE id = $3', [name, email, id]);
    if (result.rowCount === 0) {
      return res.status(404).send('User not found');
    }
    res.status(204).send();
  } catch (e) {
    console.error('Error updating user:', e);
    res.status(500).send('Internal Server Error');
  }
});

app.delete('/users/:id', async (req, res) => {
  const { id } = req.params;
  try {
    const result = await userPool.query('DELETE FROM users WHERE id = $1', [id]);
    if (result.rowCount === 0) {
      return res.status(404).send('User not found');
    }
    res.status(204).send();
  } catch (e) {
    console.error('Error deleting user:', e);
    res.status(500).send('Internal Server Error');
  }
};

// Start the server after any setup process
app.listen(port, () => {
  console.log(`Server running at http://localhost:${port}`);
});
