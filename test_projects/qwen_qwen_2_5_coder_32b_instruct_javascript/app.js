
const express = require('express');
const { Pool } = require('pg');
const bodyParser = require('body-parser');

const app = express();
app.use(bodyParser.json());

const pool = new Pool({
  host: 'host.docker.internal',
  port: 5432,
  database: 'test_qwen_qwen_2_5_coder_32b_instruct_javascript',
  user: 'postgres',
  password: process.env.PGPASSWORD
});

// Create users table if not exists
pool.on('connect', () => {
  const createTableQuery = `
    CREATE TABLE IF NOT EXISTS users (
      id SERIAL PRIMARY KEY,
      name TEXT NOT NULL,
      email TEXT NOT NULL
    )
  `;
  pool.query(createTableQuery, [], (err, res) => {
    if (err) {
      console.error('Error creating table', err.stack);
    } else {
      console.log('Users table created successfully!');
    }
  });
});

// POST /users
app.post('/users', (req, res) => {
  const { name, email } = req.body;
  const query = 'INSERT INTO users (name, email) VALUES ($1, $2) RETURNING *';
  pool.query(query, [name, email], (err, result) => {
    if (err) {
      res.status(500).send(err);
    } else {
      res.status(201).json(result.rows[0]);
    }
  });
});

// GET /users
app.get('/users', (req, res) => {
  const query = 'SELECT * FROM users';
  pool.query(query, [], (err, result) => {
    if (err) {
      res.status(500).send(err);
    } else {
      res.status(200).json(result.rows);
    }
  });
});

// GET /users/:id
app.get('/users/:id', (req, res) => {
  const { id } = req.params;
  const query = 'SELECT * FROM users WHERE id = $1';
  pool.query(query, [id], (err, result) => {
    if (err) {
      res.status(500).send(err);
    } else if (result.rows.length === 0) {
      res.status(404).send('User not found');
    } else {
      res.status(200).json(result.rows[0]);
    }
  });
});

// PUT /users/:id
app.put('/users/:id', (req, res) => {
  const { id } = req.params;
  const { name, email } = req.body;
  const query = 'UPDATE users SET name = $1, email = $2 WHERE id = $3 RETURNING *';
  pool.query(query, [name, email, id], (err, result) => {
    if (err) {
      res.status(500).send(err);
    } else if (result.rows.length === 0) {
      res.status(404).send('User not found');
    } else {
      res.status(200).json(result.rows[0]);
    }
  });
});

// DELETE /users/:id
app.delete('/users/:id', (req, res) => {
  const { id } = req.params;
  const query = 'DELETE FROM users WHERE id = $1';
  pool.query(query, [id], (err, result) => {
    if (err) {
      res.status(500).send(err);
    } else if (result.rowCount === 0) {
      res.status(404).send('User not found');
    } else {
      res.status(204).send();
    }
  });
});

const PORT = 8080;
app.listen(PORT, () => {
  console.log(`Server is running on port ${PORT}`);
});
