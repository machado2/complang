import express, { Request, Response } from "express";
import { Pool } from "pg";

const app = express();
const port = 8080;

app.use(express.json());

const pool = new Pool({
  host: "host.docker.internal",
  port: 5432,
  database: "complang",
  user: "testuser",
  password: process.env.PGPASSWORD,
});

app.post("/users", async (req: Request, res: Response) => {
  const { name, email } = req.body;
  try {
    const result = await pool.query(
      "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email",
      [name, email]
    );
    const user = result.rows[0];
    res.status(201).json(user);
  } catch (err) {
    console.error(err);
    res.status(500).send("Server error");
  }
});

app.get("/users", async (req: Request, res: Response) => {
  try {
    const result = await pool.query("SELECT id, name, email FROM users");
    res.status(200).json(result.rows);
  } catch (err) {
    console.error(err);
    res.status(500).send("Server error");
  }
});

app.get("/users/:id", async (req: Request, res: Response) => {
  const id = parseInt(req.params.id);
  try {
    const result = await pool.query(
      "SELECT id, name, email FROM users WHERE id = $1",
      [id]
    );
    if (result.rows.length === 0) {
      return res.status(404).send("User not found");
    }
    res.status(200).json(result.rows[0]);
  } catch (err) {
    console.error(err);
    res.status(500).send("Server error");
  }
});

app.put("/users/:id", async (req: Request, res: Response) => {
  const id = parseInt(req.params.id);
  const { name, email } = req.body;
  try {
    const result = await pool.query(
      "UPDATE users SET name = $1, email = $2 WHERE id = $3",
      [name, email, id]
    );
    if (result.rowCount === 0) {
      return res.status(404).send("User not found");
    }
    res.status(204).send();
  } catch (err) {
    console.error(err);
    res.status(500).send("Server error");
  }
});

app.delete("/users/:id", async (req: Request, res: Response) => {
  const id = parseInt(req.params.id);
  try {
    const result = await pool.query("DELETE FROM users WHERE id = $1", [id]);
    if (result.rowCount === 0) {
      return res.status(404).send("User not found");
    }
    res.status(204).send();
  } catch (err) {
    console.error(err);
    res.status(500).send("Server error");
  }
});

app.listen(port, () => {
  console.log(`Server is running on port ${port}`);
});
