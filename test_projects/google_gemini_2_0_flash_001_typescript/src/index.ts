
import express, { Express, Request, Response } from 'express';
import { Pool } from 'pg';

const app: Express = express();
const port = 8080;

app.use(express.json());

const dbName = 'test_google_gemini_2_0_flash_001_typescript';

async function initializeDatabase() {
  let pool: Pool | null = null;
  try {
    // Connect to the default 'postgres' database to check if our database exists
    pool = new Pool({
      host: 'host.docker.internal',
      database: 'postgres',
      user: 'postgres',
      password: process.env.PGPASSWORD,
      port: 5432,
    });

    const client = await pool.connect();
    const dbCheckResult = await client.query("SELECT 1 FROM pg_database WHERE datname = $1", [dbName]);

    if (dbCheckResult.rowCount === 0) {
      console.log("Database '" + dbName + "' does not exist. Creating...");
      try {
        await client.query('CREATE DATABASE "' + dbName + '"');
        console.log("Database '" + dbName + "' created.");
      } catch (createDbErr) {
          console.error("Error creating database (likely already exists): ", createDbErr);
      }
    }
    client.release();
    await pool.end(); // close connection to postgres db
    pool = null;
  } catch (err) {
    console.error("Error checking/creating database: " + err);
    if (pool) {
        try {
            await pool.end();
        } catch (endPoolError) {
            console.error("Error closing pool: " + endPoolError);
        }
    }
  }

  // Now connect to the target database and create the table if necessary
  try {
    pool = new Pool({
      host: 'host.docker.internal',
      database: dbName,
      user: 'postgres',
      password: process.env.PGPASSWORD,
      port: 5432,
    });

    const client = await pool.connect();
    const tableCheckResult = await client.query("SELECT EXISTS (SELECT FROM pg_tables WHERE tablename  = 'users')");
    if (!tableCheckResult.rows[0].exists) {
      console.log("Creating users table");
      await client.query('CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT NOT NULL, email TEXT NOT NULL UNIQUE)');
      console.log("Users table created");
    }
    client.release();
    await pool.end();
    pool = null;
  } catch (err) {
    console.error("Error initializing users table: " + err);
    if (pool) {
        try {
            await pool.end();
        } catch (endPoolError) {
            console.error("Error closing pool: " + endPoolError);
        }
    }


  }
}

initializeDatabase();


const poolMain = new Pool({
  host: 'host.docker.internal',
  database: dbName,
  user: 'postgres',
  password: process.env.PGPASSWORD,
  port: 5432,
});


// GET /users
app.get('/users', async (req: Request, res: Response) => {
  try {
    const client = await poolMain.connect();
    const result = await client.query('SELECT * FROM users');
    client.release();
    res.status(200).json(result.rows);
  } catch (err) {
    console.error(err);
    res.status(500).send('Error fetching users');
  }
});

// GET /users/:id
app.get('/users/:id', async (req: Request, res: Response) => {
  const id = parseInt(req.params.id);
  try {
    const client = await poolMain.connect();
    const result = await client.query('SELECT * FROM users WHERE id = $1', [id]);
    client.release();
    if (result.rows.length > 0) {
      res.status(200).json(result.rows[0]);
    } else {
      res.status(404).send('User not found');
    }
  } catch (err) {
    console.error(err);
    res.status(500).send('Error fetching user');
  }
});

// POST /users
app.post('/users', async (req: Request, res: Response) => {
  const { name, email } = req.body;
  try {
    const client = await poolMain.connect();
    const result = await client.query(
      'INSERT INTO users (name, email) VALUES ($1, $2) RETURNING *',
      [name, email]
    );
    client.release();
    res.status(201).json(result.rows[0]);
  } catch (err) {
    console.error(err);
    res.status(500).send('Error creating user');
  }
});

// PUT /users/:id
app.put('/users/:id', async (req: Request, res: Response) => {
  const id = parseInt(req.params.id);
  const { name, email } = req.body;
  try {
    const client = await poolMain.connect();
    const result = await client.query(
      'UPDATE users SET name = $1, email = $2 WHERE id = $3 RETURNING *',
      [name, email, id]
    );
    client.release();
    if (result.rows.length > 0) {
      res.status(200).send("User successfully updated");
    } else {
      res.status(404).send('User not found');
    }
  } catch (err) {
    console.error(err);
    res.status(500).send('Error updating user');
  }
});


// DELETE /users/:id
app.delete('/users/:id', async (req: Request, res: Response) => {
  const id = parseInt(req.params.id);
  try {
    const client = await poolMain.connect();
    const result = await client.query('DELETE FROM users WHERE id = $1 RETURNING *', [id]);
    client.release();
    if (result.rows.length > 0) {
        res.status(200).send("User successfully deleted");
    } else {
      res.status(404).send('User not found');
    }
  } catch (err) {
    console.error(err);
    res.status(500).send('Error deleting user');
  }
});


app.listen(port, () => {
  console.log("Server is running on port " + port);
});
