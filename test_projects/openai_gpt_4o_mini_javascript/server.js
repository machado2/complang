const express = require('express');
const { Pool } = require('pg');
const app = express();
const port = 8080;

app.use(express.json());

const pool = new Pool({
    host: 'host.docker.internal',
    port: 5432,
    database: 'complang',
    user: 'testuser',
    password: process.env.PGPASSWORD,
});

// Create a user
app.post('/users', async (req, res) => {
    const { name, email } = req.body;
    const result = await pool.query('INSERT INTO users (name, email) VALUES ($1, $2) RETURNING *', [name, email]);
    const user = result.rows[0];
    res.status(201).json(user);
});

// Get all users
app.get('/users', async (req, res) => {
    const result = await pool.query('SELECT * FROM users');
    res.status(200).json(result.rows);
});

// Get a single user
app.get('/users/:id', async (req, res) => {
    const result = await pool.query('SELECT * FROM users WHERE id = $1', [req.params.id]);
    if (result.rows.length === 0) {
        return res.status(404).json({ message: 'User not found' });
    }
    res.status(200).json(result.rows[0]);
});

// Update a user
app.put('/users/:id', async (req, res) => {
    const { name, email } = req.body;
    const result = await pool.query('UPDATE users SET name = $1, email = $2 WHERE id = $3', [name, email, req.params.id]);
    if (result.rowCount === 0) {
        return res.status(404).json({ message: 'User not found' });
    }
    res.status(204).send();
});

// Delete a user
app.delete('/users/:id', async (req, res) => {
    const result = await pool.query('DELETE FROM users WHERE id = $1', [req.params.id]);
    if (result.rowCount === 0) {
        return res.status(404).json({ message: 'User not found' });
    }
    res.status(204).send();
});

app.listen(port, () => {
    console.log(`Server running at http://localhost:${port}`);
});