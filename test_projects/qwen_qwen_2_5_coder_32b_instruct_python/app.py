
import os
import psycopg2
from psycopg2 import sql
from flask import Flask, request, jsonify

app = Flask(__name__)

DATABASE_URL = f"host=host.docker.internal dbname=test_qwen_qwen_2_5_coder_32b_instruct_python user=postgres password={os.getenv('PGPASSWORD')}"

def get_db_connection():
    conn = psycopg2.connect(DATABASE_URL)
    return conn

@app.before_first_request
def create_tables():
    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute(""" CREATE TABLE IF NOT EXISTS users (
                        id SERIAL PRIMARY KEY,
                        name TEXT,
                        email TEXT
    ) """)
    conn.commit()
    cur.close()
    conn.close()

@app.route('/users', methods=['POST'])
def create_user():
    new_user = request.get_json()
    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute('INSERT INTO users (name, email) VALUES (%s, %s) RETURNING id', (new_user['name'], new_user['email']))
    user_id = cur.fetchone()[0]
    conn.commit()
    cur.close()
    conn.close()
    return jsonify({'id': user_id, 'name': new_user['name'], 'email': new_user['email']}), 201

@app.route('/users', methods=['GET'])
def get_users():
    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute('SELECT id, name, email FROM users')
    users = cur.fetchall()
    cur.close()
    conn.close()
    return jsonify([{'id': user[0], 'name': user[1], 'email': user[2]} for user in users]), 200

@app.route('/users/<int:user_id>', methods=['GET'])
def get_user(user_id):
    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute('SELECT id, name, email FROM users WHERE id=%s', (user_id,))
    user = cur.fetchone()
    cur.close()
    conn.close()
    if user is None:
        return jsonify({'message': 'User not found'}), 404
    return jsonify({'id': user[0], 'name': user[1], 'email': user[2]}), 200

@app.route('/users/<int:user_id>', methods=['PUT'])
def update_user(user_id):
    updated_user = request.get_json()
    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute('UPDATE users SET name=%s, email=%s WHERE id=%s', (updated_user['name'], updated_user['email'], user_id))
    conn.commit()
    cur.close()
    conn.close()
    if cur.rowcount == 0:
        return jsonify({'message': 'User not found'}), 404
    return '', 204

@app.route('/users/<int:user_id>', methods=['DELETE'])
def delete_user(user_id):
    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute('DELETE FROM users WHERE id=%s', (user_id,))
    conn.commit()
    cur.close()
    conn.close()
    if cur.rowcount == 0:
        return jsonify({'message': 'User not found'}), 404
    return '', 204

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8080)
