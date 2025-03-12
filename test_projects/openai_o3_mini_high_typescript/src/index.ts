import express from 'express';
import { Pool } from 'pg';

const app = express();
app.use(express.json());

const pool = new Pool({
  host: 'host.docker.internal',
  port: 5432,
  database: 'complang',
  user: 'testuser',
  password: process.env.PGPASSWORD
});

// POST /users: Create a new user
app.post('/users', async (req, res) => {
  const { name, email } = req.body;
  if (!name || !email) {
    return res.status(400).json({ error: 'Name and email are required.' });
  }
  try {
    const result = await pool.query(
      'INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email',
      [name, email]
    );
    const newUser = result.rows[0];
    res.status(201).json(newUser);
  } catch (error) {
    console.error('Error creating user:', error);
    res.status(500).json({ error: 'Internal server error.' });
  }
});

// GET /users: Get all users
app.get('/users', async (req, res) => {
  try {
    const result = await pool.query('SELECT id, name, email FROM users');
    res.status(200).json(result.rows);
  } catch (error) {
    console.error('Error fetching users:', error);
    res.status(500).json({ error: 'Internal server error.' });
  }
});

// GET /users/:id: Get a single user by id
app.get('/users/:id', async (req, res) => {
  const userId = parseInt(req.params.id);
  if (isNaN(userId)) {
    return res.status(400).json({ error: 'Invalid user id.' });
  }
  try {
    const result = await pool.query('SELECT id, name, email FROM users WHERE id = $1', [userId]);
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
  const userId = parseInt(req.params.id);
  const { name, email } = req.body;
  if (isNaN(userId) || !name || !email) {
    return res.status(400).json({ error: 'Invalid request payload.' });
  }
  try {
    const result = await pool.query(
      'UPDATE users SET name = $1, email = $2 WHERE id = $3 RETURNING id, name, email',
      [name, email, userId]
    );
    if (result.rowCount === 0) {
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
  const userId = parseInt(req.params.id);
  if (isNaN(userId)) {
    return res.status(400).json({ error: 'Invalid user id.' });
  }
  try {
    const result = await pool.query('DELETE FROM users WHERE id = $1 RETURNING id', [userId]);
    if (result.rowCount === 0) {
      return res.status(404).json({ error: 'User not found.' });
    }
    res.status(204).send();
  } catch (error) {
    console.error('Error deleting user:', error);
    res.status(500).json({ error: 'Internal server error.' });
  }
});

app.listen(8080, () => {
  console.log('Server is running on port 8080');
});
