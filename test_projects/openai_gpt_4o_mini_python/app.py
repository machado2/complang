
from flask import Flask, request, jsonify
import psycopg2
import os
import time

app = Flask(__name__)

# Database connection parameters
DB_HOST = 'host.docker.internal'
DB_PORT = '5432'
DB_NAME = 'test_openai_gpt_4o_mini_python'
DB_USER = 'postgres'
DB_PASSWORD = os.getenv('PGPASSWORD')

# Establish a connection to the database with retries
def get_db_connection():
    for i in range(5):  # Retry connecting 5 times
        try:
            conn = psycopg2.connect(host=DB_HOST, port=DB_PORT, database=DB_NAME, user=DB_USER, password=DB_PASSWORD)
            return conn
        except psycopg2.OperationalError:
            time.sleep(2)  # Wait before retrying
    raise Exception("Could not connect to the database after several attempts.")

# Create the users table if it doesn't exist
def create_table():
    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute('''
        CREATE TABLE IF NOT EXISTS users (
            id SERIAL PRIMARY KEY,
            name TEXT NOT NULL,
            email TEXT NOT NULL
        )
    ''')
    conn.commit()
    cur.close()
    conn.close()

@app.route('/users', methods=['POST'])
def create_user():
    data = request.get_json()
    name = data.get('name')
    email = data.get('email')

    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute('INSERT INTO users (name, email) VALUES (%s, %s) RETURNING id', (name, email))
    user_id = cur.fetchone()[0]
    conn.commit()
    cur.close()
    conn.close()

    return jsonify(id=user_id, name=name, email=email), 201

@app.route('/users', methods=['GET'])
def get_users():
    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute('SELECT id, name, email FROM users')
    users = cur.fetchall()
    cur.close()
    conn.close()

    return jsonify([{'id': u[0], 'name': u[1], 'email': u[2]} for u in users]), 200

@app.route('/users/<int:user_id>', methods=['GET'])
def get_user(user_id):
    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute('SELECT id, name, email FROM users WHERE id = %s', (user_id,))
    user = cur.fetchone()
    cur.close()
    conn.close()

    if user is None:
        return jsonify({'error': 'User not found'}), 404

    return jsonify({'id': user[0], 'name': user[1], 'email': user[2]}), 200

@app.route('/users/<int:user_id>', methods=['PUT'])
def update_user(user_id):
    data = request.get_json()
    name = data.get('name')
    email = data.get('email')

    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute('UPDATE users SET name = %s, email = %s WHERE id = %s', (name, email, user_id))
    conn.commit()
    cur.close()
    conn.close()

    return '', 204

@app.route('/users/<int:user_id>', methods=['DELETE'])
def delete_user(user_id):
    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute('DELETE FROM users WHERE id = %s', (user_id,))
    conn.commit()
    cur.close()
    conn.close()

    return '', 204

if __name__ == '__main__':
    create_table()  # Create table on startup
    app.run(host='0.0.0.0', port=8080)
