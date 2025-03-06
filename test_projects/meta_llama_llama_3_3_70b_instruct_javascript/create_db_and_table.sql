
CREATE DATABASE IF NOT EXISTS test_meta_llama_llama_3_3_70b_instruct_javascript;
\c test_meta_llama_llama_3_3_70b_instruct_javascript
CREATE TABLE IF NOT EXISTS users (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  email TEXT NOT NULL
);
