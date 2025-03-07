
import express from 'express';
import { Pool } from 'pg';
import dotenv from 'dotenv';

dotenv.config();

const app = express();
app.use(express.json());
const port = 8080;

const pool = new Pool({
    user: 'testuser',
    host: 'host.docker.internal',
    database: 'complang',
    password: process.env.PGPASSWORD,
    port: 5432,
});

// Create a user
app.post('/users', async (req, res) => {
    const { name, email } = req.body;
    const result = await pool.query('INSERT INTO users (name, email) VALUES ($1, $2) RETURNING *', [name, email]);
    res.status(201).json(result.rows[0]);
});

// Get all users
app.get('/users', async (req, res) => {
    const result = await pool.query('SELECT * FROM users');
    res.status(200).json(result.rows);
});

// Get a user by ID
app.get('/users/:id', async (req, res) => {
    const { id } = req.params;
    const result = await pool.query('SELECT * FROM users WHERE id = $1', [id]);
    if (result.rows.length > 0) {
        res.status(200).json(result.rows[0]);
    } else {
        res.status(404).send();
    }
});

// Update a user
app.put('/users/:id', async (req, res) => {
    const { id } = req.params;
    const { name, email } = req.body;
    const result = await pool.query('UPDATE users SET name = $1, email = $2 WHERE id = $3', [name, email, id]);
    if (result.rowCount !== undefined && result.rowCount > 0) {
        res.status(204).send();
    } else {
        res.status(404).send();
    }
});

// Delete a user
app.delete('/users/:id', async (req, res) => {
    const { id } = req.params;
    const result = await pool.query('DELETE FROM users WHERE id = $1', [id]);
    if (result.rowCount !== undefined && result.rowCount > 0) {
        res.status(204).send();
    } else {
        res.status(404).send();
    }
});

app.listen(port, () => {
    console.log(`Server running at http://localhost:${port}`);
});
