from flask import Flask, request, jsonify
import os
import psycopg2

app = Flask(__name__)

# Database connection
DATABASE_URL = os.environ.get('DATABASE_URL', 'postgresql://testuser:Saloon5-Moody-Observing@host.docker.internal:5432/complang')

conn = psycopg2.connect(DATABASE_URL)
cursor = conn.cursor()

# Create User
@app.route('/users', methods=['POST'])
def create_user():
    data = request.json
    name = data.get('name')
    email = data.get('email')
    cursor.execute('INSERT INTO users (name, email) VALUES (%s, %s) RETURNING id;', (name, email))
    user_id = cursor.fetchone()[0]
    conn.commit()
    return jsonify({'id': user_id, 'name': name, 'email': email}), 201

# Get All Users
@app.route('/users', methods=['GET'])
def get_users():
    cursor.execute('SELECT * FROM users;')
    users = [{'id': row[0], 'name': row[1], 'email': row[2]} for row in cursor.fetchall()]
    return jsonify(users), 200

# Get User by ID
@app.route('/users/<int:user_id>', methods=['GET'])
def get_user(user_id):
    cursor.execute('SELECT * FROM users WHERE id = %s;', (user_id,))
    user = cursor.fetchone()
    if user:
        return jsonify({'id': user[0], 'name': user[1], 'email': user[2]}), 200
    return jsonify({'error': 'User not found'}), 404

# Update User
@app.route('/users/<int:user_id>', methods=['PUT'])
def update_user(user_id):
    data = request.json
    name = data.get('name')
    email = data.get('email')
    cursor.execute('UPDATE users SET name = %s, email = %s WHERE id = %s;', (name, email, user_id))
    conn.commit()
    if cursor.rowcount:
        return jsonify({'message': 'User updated'}), 200
    return jsonify({'error': 'User not found'}), 404

# Delete User
@app.route('/users/<int:user_id>', methods=['DELETE'])
def delete_user(user_id):
    cursor.execute('DELETE FROM users WHERE id = %s;', (user_id,))
    conn.commit()
    if cursor.rowcount:
        return jsonify({'message': 'User deleted'}), 200
    return jsonify({'error': 'User not found'}), 404

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8080)