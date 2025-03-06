
from flask import Flask, request, jsonify
import os
import psycopg2

app = Flask(__name__)

host = os.environ['PGHOST']
database = 'test_meta_llama_llama_3_3_70b_instruct_python'
user = 'postgres'
password = os.environ['PGPASSWORD']

def get_db_connection():
    conn = psycopg2.connect(host=host, database=database, user=user, password=password)
    return conn

def create_table():
    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute('CREATE TABLE IF NOT EXISTS users (id SERIAL PRIMARY KEY, name TEXT, email TEXT);')
    conn.commit()
    cur.close()
    conn.close()

create_table()

@app.route('/users', methods=['POST'])
def create_user():
    data = request.get_json()
    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute('INSERT INTO users (name, email) VALUES (%s, %s) RETURNING *;', (data['name'], data['email']))
    user = cur.fetchone()
    conn.commit()
    cur.close()
    conn.close()
    return jsonify({'id': user[0], 'name': user[1], 'email': user[2]}), 201

@app.route('/users', methods=['GET'])
def get_users():
    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute('SELECT * FROM users;')
    users = cur.fetchall()
    conn.commit()
    cur.close()
    conn.close()
    return jsonify([{'id': user[0], 'name': user[1], 'email': user[2]} for user in users])

@app.route('/users/<int:user_id>', methods=['GET'])
def get_user(user_id):
    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute('SELECT * FROM users WHERE id = %s;', (user_id,))
    user = cur.fetchone()
    conn.commit()
    cur.close()
    conn.close()
    if user is None:
        return jsonify({}), 404
    return jsonify({'id': user[0], 'name': user[1], 'email': user[2]})

@app.route('/users/<int:user_id>', methods=['PUT'])
def update_user(user_id):
    data = request.get_json()
    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute('SELECT * FROM users WHERE id = %s;', (user_id,))
    user = cur.fetchone()
    if user is None:
        return jsonify({}), 404
    cur.execute('UPDATE users SET name = %s, email = %s WHERE id = %s;', (data['name'], data['email'], user_id))
    conn.commit()
    cur.close()
    conn.close()
    return jsonify({})

@app.route('/users/<int:user_id>', methods=['DELETE'])
def delete_user(user_id):
    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute('SELECT * FROM users WHERE id = %s;', (user_id,))
    user = cur.fetchone()
    if user is None:
        return jsonify({}), 404
    cur.execute('DELETE FROM users WHERE id = %s;', (user_id,))
    conn.commit()
    cur.close()
    conn.close()
    return jsonify({})

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8080)
