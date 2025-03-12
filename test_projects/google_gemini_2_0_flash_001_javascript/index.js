
const express = require('express');
const { Pool } = require('pg');

const app = express();
const port = 8080;

app.use(express.json());

const pool = new Pool({
  host: 'host.docker.internal',
  user: 'testuser',
  database: 'complang',
  password: process.env.PGPASSWORD,
  port: 5432,
});

app.post('/users', async (req, res) => {
  const { name, email } = req.body;
  try {
    const result = await pool.query(
      'INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email',
      [name, email]
    );
    res.status(201).json(result.rows[0]);
  } catch (err) {
    console.error(err);
    res.status(500).send('Error creating user');
  }
});

app.get('/users', async (req, res) => {
  try {
    const result = await pool.query('SELECT id, name, email FROM users');
    res.status(200).json(result.rows);
  } catch (err) {
    console.error(err);
    res.status(500).send('Error retrieving users');
  }
});

app.get('/users/:id', async (req, res) => {
  const { id } = req.params;
  try {
    const result = await pool.query(
      'SELECT id, name, email FROM users WHERE id = $1',
      [id]
    );
    if (result.rows.length === 0) {
      return res.status(404).send('User not found');
    }
    res.status(200).json(result.rows[0]);
  } catch (err) {
    console.error(err);
    res.status(500).send('Error retrieving user');
  }
});

app.put('/users/:id', async (req, res) => {
  const { id } = req.params;
  const { name, email } = req.body;
  try {
    const result = await pool.query(
      'UPDATE users SET name = $1, email = $2 WHERE id = $3',
      [name, email, id]
    );
    if (result.rowCount === 0) {
      return res.status(404).send('User not found');
    }
    res.status(204).send();
  } catch (err) {
    console.error(err);
    res.status(500).send('Error updating user');
  }
});

app.delete('/users/:id', async (req, res) => {
  const { id } = req.params;
  try {
    const result = await pool.query('DELETE FROM users WHERE id = $1', [id]);
    if (result.rowCount === 0) {
      return res.status(404).send('User not found');
    }
    res.status(204).send();
  } catch (err) {
    console.error(err);
    res.status(500).send('Error deleting user');
  }
});

app.listen(port, () => {
  console.log('Server listening on port ' + port);
});
