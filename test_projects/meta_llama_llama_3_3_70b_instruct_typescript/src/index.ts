
import express, { Request, Response } from 'express';
import { Pool } from 'pg';

const app = express();
app.use(express.json());

const pool = new Pool({
  user: 'testuser',
  host: 'host.docker.internal',
  database: 'complang',
  password: process.env.PGPASSWORD,
  port: 5432,
});

app.post('/users', async (req: Request, res: Response) => {
  const { name, email } = req.body;
  const result = await pool.query('INSERT INTO users (name, email) VALUES ($1, $2) RETURNING *', [name, email]);
  res.status(201).send(result.rows[0]);
});

app.get('/users', async (req: Request, res: Response) => {
  const result = await pool.query('SELECT * FROM users');
  res.send(result.rows);
});

app.get('/users/:id', async (req: Request, res: Response) => {
  const id = req.params.id;
  const result = await pool.query('SELECT * FROM users WHERE id = $1', [id]);
  if (result.rows.length === 0) {
    res.status(404).send({ message: 'User not found' });
  } else {
    res.send(result.rows[0]);
  }
});

app.put('/users/:id', async (req: Request, res: Response) => {
  const id = req.params.id;
  const { name, email } = req.body;
  const result = await pool.query('UPDATE users SET name = $1, email = $2 WHERE id = $3 RETURNING *', [name, email, id]);
  if (result.rows.length === 0) {
    res.status(404).send({ message: 'User not found' });
  } else {
    res.send(result.rows[0]);
  }
});

app.delete('/users/:id', async (req: Request, res: Response) => {
  const id = req.params.id;
  const result = await pool.query('DELETE FROM users WHERE id = $1', [id]);
  if (result.rowCount === 0) {
    res.status(404).send({ message: 'User not found' });
  } else {
    res.status(204).send();
  }
});

app.listen(8080, () => {
  console.log('Server listening on port 8080');
});
