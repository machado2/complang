const { Pool } = require('pg');

const pool = new Pool({
  host: 'host.docker.internal',
  port: 5432,
  database: 'complang',
  user: 'testuser',
  password: process.env.PGPASSWORD
});

module.exports = {
  query: (text, params) => pool.query(text, params),
};
