import { Pool } from 'pg';

// Create a connection pool to the PostgreSQL database
const pool = new Pool({
  host: 'host.docker.internal',
  port: 5432,
  database: 'complang',
  user: 'testuser',
  password: process.env.PGPASSWORD,
});

// Function to check if connection is successful
export const checkConnection = async () => {
  try {
    const client = await pool.connect();
    console.log('Database connection successful');
    client.release();
    return true;
  } catch (error) {
    console.error('Database connection error:', error);
    return false;
  }
};

export default pool;
