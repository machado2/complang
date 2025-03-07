import express from 'express';
import { Pool } from 'pg';

const app = express();
app.use(express.json());

// Create a PostgreSQL connection pool
const pool = new Pool({
  user: 'testuser',
  host: 'host.docker.internal',
  database: 'complang',
  password: process.env.PGPASSWORD,
  port: 5432
});

// POST /users: Create a new user
app.post('/users', async (req, res) => {
  const { name, email } = req.body;
  if (!name || !email) {
    return res.status(400).json({ error: 'Name and email are required.' });
  }
  try {
    const result = await pool.query(
      'INSERT INTO users (name, email) VALUES ($1, $2) RETURNING *',
      [name, email]
    );
    return res.status(201).json(result.rows[0]);
  } catch (error) {
    console.error('Error creating user', error);
    return res.status(500).json({ error: 'Internal server error' });
  }
});

// GET /users: Get all users
app.get('/users', async (req, res) => {
  try {
    const result = await pool.query('SELECT * FROM users');
    return res.status(200).json(result.rows);
  } catch (error) {
    console.error('Error fetching users', error);
    return res.status(500).json({ error: 'Internal server error' });
  }
});

// GET /users/:id: Get a user by ID
app.get('/users/:id', async (req, res) => {
  const userId = parseInt(req.params.id, 10);
  try {
    const result = await pool.query('SELECT * FROM users WHERE id = $1', [userId]);
    if (result.rows.length === 0) {
      return res.status(404).json({ error: 'User not found' });
    }
    return res.status(200).json(result.rows[0]);
  } catch (error) {
    console.error('Error fetching user', error);
    return res.status(500).json({ error: 'Internal server error' });
  }
});

// PUT /users/:id: Update a user
app.put('/users/:id', async (req, res) => {
  const userId = parseInt(req.params.id, 10);
  const { name, email } = req.body;
  if (!name || !email) {
    return res.status(400).json({ error: 'Name and email are required.' });
  }
  try {
    const result = await pool.query(
      'UPDATE users SET name = $1, email = $2 WHERE id = $3 RETURNING *',
      [name, email, userId]
    );
    if (result.rows.length === 0) {
      return res.status(404).json({ error: 'User not found' });
    }
    return res.status(200).json(result.rows[0]);
  } catch (error) {
    console.error('Error updating user', error);
    return res.status(500).json({ error: 'Internal server error' });
  }
});

// DELETE /users/:id: Delete a user
app.delete('/users/:id', async (req, res) => {
  const userId = parseInt(req.params.id, 10);
  try {
    const result = await pool.query(
      'DELETE FROM users WHERE id = $1 RETURNING *',
      [userId]
    );
    if (result.rows.length === 0) {
      return res.status(404).json({ error: 'User not found' });
    }
    // 204 No Content indicates successful deletion with no response body
    return res.status(204).send();
  } catch (error) {
    console.error('Error deleting user', error);
    return res.status(500).json({ error: 'Internal server error' });
  }
});

// Start the server on port 8080
app.listen(8080, () => {
  console.log('Server is running on port 8080');
});
