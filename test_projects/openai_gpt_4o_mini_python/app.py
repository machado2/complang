from flask import Flask, request, jsonify
import os
import psycopg2

app = Flask(__name__)

# Database connection
def get_db_connection():
    conn = psycopg2.connect(
        host='host.docker.internal',
        database='complang',
        user='testuser',
        password=os.getenv('PGPASSWORD')
    )
    return conn

@app.route('/users', methods=['POST'])
def create_user():
    user_data = request.json
    name = user_data['name']
    email = user_data['email']
    
    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute('INSERT INTO users (name, email) VALUES (%s, %s) RETURNING id;', (name, email))
    user_id = cur.fetchone()[0]
    conn.commit()
    cur.close()
    conn.close()
    
    return jsonify({"id": user_id, "name": name, "email": email}), 201

@app.route('/users', methods=['GET'])
def get_users():
    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute('SELECT id, name, email FROM users;')
    users = cur.fetchall()
    cur.close()
    conn.close()

    return jsonify([{"id": user[0], "name": user[1], "email": user[2]} for user in users]), 200

@app.route('/users/<int:user_id>', methods=['GET'])
def get_user(user_id):
    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute('SELECT id, name, email FROM users WHERE id = %s;', (user_id,))
    user = cur.fetchone()
    cur.close()
    conn.close()

    if user:
        return jsonify({"id": user[0], "name": user[1], "email": user[2]}), 200
    else:
        return jsonify({'error': 'User not found'}), 404

@app.route('/users/<int:user_id>', methods=['PUT'])
def update_user(user_id):
    user_data = request.json
    name = user_data['name']
    email = user_data['email']

    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute('UPDATE users SET name = %s, email = %s WHERE id = %s;', (name, email, user_id))
    updated_rows = cur.rowcount
    conn.commit()
    cur.close()
    conn.close()

    if updated_rows > 0:
        return jsonify({'message': 'User updated successfully'}), 200
    else:
        return jsonify({'error': 'User not found'}), 404

@app.route('/users/<int:user_id>', methods=['DELETE'])
def delete_user(user_id):
    conn = get_db_connection()
    cur = conn.cursor()
    cur.execute('DELETE FROM users WHERE id = %s;', (user_id,))
    deleted_rows = cur.rowcount
    conn.commit()
    cur.close()
    conn.close()

    if deleted_rows > 0:
        return jsonify({'message': 'User deleted successfully'}), 200
    else:
        return jsonify({'error': 'User not found'}), 404

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8080)
