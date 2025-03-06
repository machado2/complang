
const express = require('express');
const { Pool } = require('pg');
const app = express();
app.use(express.json());

const pool = new Pool({
  user: 'postgres',
  host: 'host.docker.internal',
  database: 'test_meta_llama_llama_3_3_70b_instruct_javascript',
  password: process.env.PGPASSWORD,
  port: 5432,
});

app.post('/users', (req, res) => {
  const { name, email } = req.body;
  pool.query('INSERT INTO users (name, email) VALUES ($1, $2) RETURNING *', [name, email], (err, results) => {
    if (err) {
      res.status(500).send({ message: 'Error creating user' });
    } else {
      res.status(201).send(results.rows[0]);
    }
  });
});

app.get('/users', (req, res) => {
  pool.query('SELECT * FROM users', (err, results) => {
    if (err) {
      res.status(500).send({ message: 'Error fetching users' });
    } else {
      res.status(200).send(results.rows);
    }
  });
});

app.get('/users/:id', (req, res) => {
  const id = req.params.id;
  pool.query('SELECT * FROM users WHERE id = $1', [id], (err, results) => {
    if (err) {
      res.status(500).send({ message: 'Error fetching user' });
    } else if (results.rows.length === 0) {
      res.status(404).send({ message: 'User not found' });
    } else {
      res.status(200).send(results.rows[0]);
    }
  });
});

app.put('/users/:id', (req, res) => {
  const id = req.params.id;
  const { name, email } = req.body;
  pool.query('UPDATE users SET name = $1, email = $2 WHERE id = $3 RETURNING *', [name, email, id], (err, results) => {
    if (err) {
      res.status(500).send({ message: 'Error updating user' });
    } else if (results.rows.length === 0) {
      res.status(404).send({ message: 'User not found' });
    } else {
      res.status(200).send(results.rows[0]);
    }
  });
});

app.delete('/users/:id', (req, res) => {
  const id = req.params.id;
  pool.query('DELETE FROM users WHERE id = $1', [id], (err, results) => {
    if (err) {
      res.status(500).send({ message: 'Error deleting user' });
    } else if (results.rowCount === 0) {
      res.status(404).send({ message: 'User not found' });
    } else {
      res.status(204).send();
    }
  });
});

app.listen(8080, () => {
  console.log('Server listening on port 8080');
});

const sql = `
  CREATE TABLE IF NOT EXISTS users (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    email TEXT NOT NULL
  );
`;

pool.query(sql, (err, results) => {
  if (err) {
    console.error('Error creating table:', err);
  } else {
    console.log('Table created successfully');
  }
});
