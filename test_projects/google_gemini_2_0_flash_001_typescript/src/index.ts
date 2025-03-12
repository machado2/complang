
import express, { Request, Response } from 'express';
import { Pool } from 'pg';
import dotenv from 'dotenv';

dotenv.config();

const app = express();
const port = 8080;

app.use(express.json());

const dbConfig = {
    user: process.env.DB_USER || 'testuser',
    host: process.env.DB_HOST || 'host.docker.internal',
    database: process.env.DB_NAME || 'complang',
    password: process.env.PGPASSWORD || 'Saloon5-Moody-Observing',
    port: Number(process.env.DB_PORT) || 5432,
};

const pool = new Pool(dbConfig);

app.post('/users', async (req: Request, res: Response) => {
    try {
        const { name, email } = req.body;
        const result = await pool.query(
            'INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email',
            [name, email]
        );
        res.status(201).json(result.rows[0]);
    } catch (err) {
        console.error(err);
        res.status(500).send('Server error');
    }
});

app.get('/users', async (req: Request, res: Response) => {
    try {
        const result = await pool.query('SELECT id, name, email FROM users');
        res.status(200).json(result.rows);
    } catch (err) {
        console.error(err);
        res.status(500).send('Server error');
    }
});

app.get('/users/:id', async (req: Request, res: Response) => {
    try {
        const { id } = req.params;
        const result = await pool.query('SELECT id, name, email FROM users WHERE id = $1', [id]);
        if (result.rows.length === 0) {
            return res.status(404).send('User not found');
        }
        res.status(200).json(result.rows[0]);
    } catch (err) {
        console.error(err);
        res.status(500).send('Server error');
    }
});

app.put('/users/:id', async (req: Request, res: Response) => {
    try {
        const { id } = req.params;
        const { name, email } = req.body;
        const result = await pool.query(
            'UPDATE users SET name = $1, email = $2 WHERE id = $3',
            [name, email, id]
        );
        if (result.rowCount === 0) {
            return res.status(404).send('User not found');
        }
        res.status(204).send('');
    } catch (err) {
        console.error(err);
        res.status(500).send('Server error');
    }
});

app.delete('/users/:id', async (req: Request, res: Response) => {
    try {
        const { id } = req.params;
        const result = await pool.query('DELETE FROM users WHERE id = $1', [id]);
        if (result.rowCount === 0) {
            return res.status(404).send('User not found');
        }
        res.status(204).send('');
    } catch (err) {
        console.error(err);
        res.status(500).send('Server error');
    }
});

app.listen(port, () => {
    console.log(`Server listening on port ${port}`);
});
