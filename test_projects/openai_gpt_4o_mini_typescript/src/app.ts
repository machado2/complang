import express, { Request, Response } from 'express';
import { Pool } from 'pg';

const app = express();
const port = 8080;
app.use(express.json());

const pool = new Pool({
  host: 'host.docker.internal',
  port: 5432,
  user: 'testuser',
  password: process.env.PGPASSWORD,
  database: 'complang'
});

// CREATE user
app.post('/users', async (req: Request, res: Response) => {
  const { name, email } = req.body;
  const result = await pool.query('INSERT INTO users (name, email) VALUES ($1, $2) RETURNING *', [name, email]);
  res.status(201).json(result.rows[0]);
});

// READ all users
app.get('/users', async (_req: Request, res: Response) => {
  const result = await pool.query('SELECT * FROM users');
  res.json(result.rows);
});

// READ single user
app.get('/users/:id', async (req: Request, res: Response) => {
  const { id } = req.params;
  const result = await pool.query('SELECT * FROM users WHERE id = $1', [id]);
  if (result.rows.length === 0) {
    return res.status(404).send('User not found');
  }
  res.json(result.rows[0]);
});

// UPDATE user
app.put('/users/:id', async (req: Request, res: Response) => {
  const { id } = req.params;
  const { name, email } = req.body;
  const result = await pool.query('UPDATE users SET name = $1, email = $2 WHERE id = $3', [name, email, id]);
  if (result.rowCount === 0) {
    return res.status(404).send('User not found');
  }
  res.sendStatus(204);
});

// DELETE user
app.delete('/users/:id', async (req: Request, res: Response) => {
  const { id } = req.params;
  const result = await pool.query('DELETE FROM users WHERE id = $1', [id]);
  if (result.rowCount === 0) {
    return res.status(404).send('User not found');
  }
  res.sendStatus(204);
});

app.listen(port, () => {
  console.log(`Server running at http://localhost:${port}`);
});