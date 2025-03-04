
const express = require('express');
const { Pool } = require('pg');

const app = express();
const port = 8080;

app.use(express.json());

const pool = new Pool({
  host: process.env.POSTGRES_HOST,
  port: process.env.POSTGRES_PORT,
  database: process.env.POSTGRES_DB,
  user: process.env.POSTGRES_USER,
  password: process.env.PGPASSWORD
});

async function initializeDatabase() {
  try {
    const client = await pool.connect();
    const tableExistsResult = await client.query(
      "SELECT EXISTS (SELECT FROM pg_tables WHERE tablename  = 'users')"
    );

    if (!tableExistsResult.rows[0].exists) {
      console.log("Creating the 'users' table");
      await client.query(`
        CREATE TABLE users (
          id SERIAL PRIMARY KEY,
          name TEXT NOT NULL,
          email TEXT NOT NULL
        )
      `);
      console.log("Table 'users' created successfully");
    } else {
      console.log("Table 'users' already exists");
    }
    client.release();
  } catch (err) {
    console.error("Error initializing database:", err);
  }
}

initializeDatabase();

// POST /users
app.post('/users', async (req, res) => {
  try {
    const { name, email } = req.body;
    const result = await pool.query(
      'INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email',
      [name, email]
    );
    const user = result.rows[0];
    res.status(201).json(user);
  } catch (err) {
    console.error(err);
    res.status(500).send('Server error');
  }
});

// GET /users
app.get('/users', async (req, res) => {
  try {
    const result = await pool.query('SELECT id, name, email FROM users');
    const users = result.rows;
    res.status(200).json(users);
  } catch (err) {
    console.error(err);
    res.status(500).send('Server error');
  }
});

// GET /users/{id}
app.get('/users/:id', async (req, res) => {
  try {
    const { id } = req.params;
    const result = await pool.query(
      'SELECT id, name, email FROM users WHERE id = $1',
      [id]
    );
    const user = result.rows[0];

    if (user) {
      res.status(200).json(user);
    } else {
      res.status(404).send('User not found');
    }
  } catch (err) {
    console.error(err);
    res.status(500).send('Server error');
  }
});

// PUT /users/{id}
app.put('/users/:id', async (req, res) => {
  try {
    const { id } = req.params;
    const { name, email } = req.body;
    const result = await pool.query(
      'UPDATE users SET name = $1, email = $2 WHERE id = $3 RETURNING id, name, email',
      [name, email, id]
    );
    const user = result.rows[0];

    if (user) {
      res.status(200).json(user); // Or 204 No Content
    } else {
      res.status(404).send('User not found');
    }
  } catch (err) {
    console.error(err);
    res.status(500).send('Server error');
  }
});


// DELETE /users/{id}
app.delete('/users/:id', async (req, res) => {
  try {
    const { id } = req.params;
    const result = await pool.query(
      'DELETE FROM users WHERE id = $1 RETURNING id',
      [id]
    );
    const user = result.rows[0];

    if (user) {
      res.status(200).send("User deleted"); // Or 204 No Content
    } else {
      res.status(404).send('User not found');
    }
  } catch (err) {
    console.error(err);
    res.status(500).send('Server error');
  }
});


app.listen(port, () => {
  console.log("Server listening on port " + port);
});
