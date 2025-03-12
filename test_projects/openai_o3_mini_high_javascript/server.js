const express = require('express');
const { Pool } = require('pg');

const app = express();
app.use(express.json());

// Set up PostgreSQL connection pool
const pool = new Pool({
  host: 'host.docker.internal',
  port: 5432,
  user: 'testuser',
  password: process.env.PGPASSWORD,
  database: 'complang'
});

// POST /users: Create a new user
app.post('/users', async (req, res) => {
  try {
    const { name, email } = req.body;
    if (!name || !email) {
      return res.status(400).json({ error: 'Name and email are required.' });
    }
    const result = await pool.query(
      'INSERT INTO users (name, email) VALUES ($1, $2) RETURNING *',
      [name, email]
    );
    res.status(201).json(result.rows[0]);
  } catch (error) {
    console.error('Error creating user:', error);
    res.status(500).json({ error: 'Internal server error.' });
  }
});

// GET /users: Retrieve all users
app.get('/users', async (req, res) => {
  try {
    const result = await pool.query('SELECT * FROM users');
    res.status(200).json(result.rows);
  } catch (error) {
    console.error('Error fetching users:', error);
    res.status(500).json({ error: 'Internal server error.' });
  }
});

// GET /users/:id: Retrieve a user by id
app.get('/users/:id', async (req, res) => {
  try {
    const userId = parseInt(req.params.id);
    const result = await pool.query('SELECT * FROM users WHERE id = $1', [userId]);
    if (result.rows.length === 0) {
      return res.status(404).json({ error: 'User not found.' });
    }
    res.status(200).json(result.rows[0]);
  } catch (error) {
    console.error('Error fetching user:', error);
    res.status(500).json({ error: 'Internal server error.' });
  }
});

// PUT /users/:id: Update a user by id
app.put('/users/:id', async (req, res) => {
  try {
    const userId = parseInt(req.params.id);
    const { name, email } = req.body;
    if (!name || !email) {
      return res.status(400).json({ error: 'Name and email are required for update.' });
    }
    const result = await pool.query(
      'UPDATE users SET name = $1, email = $2 WHERE id = $3 RETURNING *',
      [name, email, userId]
    );
    if (result.rows.length === 0) {
      return res.status(404).json({ error: 'User not found.' });
    }
    res.status(200).json(result.rows[0]);
  } catch (error) {
    console.error('Error updating user:', error);
    res.status(500).json({ error: 'Internal server error.' });
  }
});

// DELETE /users/:id: Delete a user by id
app.delete('/users/:id', async (req, res) => {
  try {
    const userId = parseInt(req.params.id);
    const result = await pool.query('DELETE FROM users WHERE id = $1 RETURNING *', [userId]);
    if (result.rows.length === 0) {
      return res.status(404).json({ error: 'User not found.' });
    }
    res.status(200).json({ message: 'User deleted successfully.' });
  } catch (error) {
    console.error('Error deleting user:', error);
    res.status(500).json({ error: 'Internal server error.' });
  }
});

const PORT = 8080;
app.listen(PORT, () => {
  console.log(`Server listening on port ${PORT}`);
});
