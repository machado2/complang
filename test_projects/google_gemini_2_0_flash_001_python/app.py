
from flask import Flask, request, jsonify
import psycopg2
from psycopg2 import OperationalError

app = Flask(__name__)

# Database configuration
db_host = 'host.docker.internal'
db_port = 5432
db_name = 'complang'
db_user = 'testuser'
db_password = 'Saloon5-Moody-Observing'

def create_connection():
    conn = None
    try:
        conn = psycopg2.connect(
            host=db_host,
            port=db_port,
            database=db_name,
            user=db_user,
            password=db_password
        )
    except OperationalError as e:
        print(f"Error connecting to the database: {e}")
    return conn

# --- Routes ---

# POST /users: Creates a user
@app.route('/users', methods=['POST'])
def create_user():
    data = request.get_json()
    name = data.get('name')
    email = data.get('email')

    if not name or not email:
        return jsonify({'message': 'Name and email are required'}), 400

    conn = create_connection()
    if conn:
        try:
            cur = conn.cursor()
            cur.execute("INSERT INTO users (name, email) VALUES (%s, %s) RETURNING id, name, email", (name, email))
            user = cur.fetchone()
            conn.commit()
            cur.close()
            conn.close()
            return jsonify({'id': user[0], 'name': user[1], 'email': user[2]}), 201
        except Exception as e:
            print(f"Error creating user: {e}")
            conn.rollback()
            cur.close()
            conn.close()
            return jsonify({'message': 'Could not create user'}), 500
    else:
        return jsonify({'message': 'Database connection error'}), 500

# GET /users: Returns a list of all users
@app.route('/users', methods=['GET'])
def get_users():
    conn = create_connection()
    if conn:
        try:
            cur = conn.cursor()
            cur.execute("SELECT id, name, email FROM users")
            users = cur.fetchall()
            cur.close()
            conn.close()
            user_list = [{'id': user[0], 'name': user[1], 'email': user[2]} for user in users]
            return jsonify(user_list), 200
        except Exception as e:
            print(f"Error getting users: {e}")
            cur.close()
            conn.close()
            return jsonify({'message': 'Could not retrieve users'}), 500
    else:
        return jsonify({'message': 'Database connection error'}), 500

# GET /users/{id}: Returns a single user
@app.route('/users/<int:user_id>', methods=['GET'])
def get_user(user_id):
    conn = create_connection()
    if conn:
        try:
            cur = conn.cursor()
            cur.execute("SELECT id, name, email FROM users WHERE id = %s", (user_id,))
            user = cur.fetchone()
            cur.close()
            conn.close()
            if user:
                return jsonify({'id': user[0], 'name': user[1], 'email': user[2]}), 200
            else:
                return jsonify({'message': 'User not found'}), 404
        except Exception as e:
            print(f"Error getting user: {e}")
            cur.close()
            conn.close()
            return jsonify({'message': 'Could not retrieve user'}), 500
    else:
        return jsonify({'message': 'Database connection error'}), 500

# PUT /users/{id}: Updates a user
@app.route('/users/<int:user_id>', methods=['PUT'])
def update_user(user_id):
    data = request.get_json()
    name = data.get('name')
    email = data.get('email')

    if not name or not email:
        return jsonify({'message': 'Name and email are required'}), 400

    conn = create_connection()
    if conn:
        try:
            cur = conn.cursor()
            cur.execute("UPDATE users SET name = %s, email = %s WHERE id = %s RETURNING id", (name, email, user_id))
            updated_user_id = cur.fetchone()
            conn.commit()
            cur.close()
            conn.close()
            if updated_user_id:
                return '', 204  # No content
            else:
                return jsonify({'message': 'User not found'}), 404
        except Exception as e:
            print(f"Error updating user: {e}")
            conn.rollback()
            cur.close()
            conn.close()
            return jsonify({'message': 'Could not update user'}), 500
    else:
        return jsonify({'message': 'Database connection error'}), 500

# DELETE /users/{id}: Deletes a user
@app.route('/users/<int:user_id>', methods=['DELETE'])
def delete_user(user_id):
    conn = create_connection()
    if conn:
        try:
            cur = conn.cursor()
            cur.execute("DELETE FROM users WHERE id = %s RETURNING id", (user_id,))
            deleted_user_id = cur.fetchone()
            conn.commit()
            cur.close()
            conn.close()
            if deleted_user_id:
                return '', 204  # No content
            else:
                return jsonify({'message': 'User not found'}), 404
        except Exception as e:
            print(f"Error deleting user: {e}")
            conn.rollback()
            cur.close()
            conn.close()
            return jsonify({'message': 'Could not delete user'}), 500
    else:
        return jsonify({'message': 'Database connection error'}), 500

if __name__ == '__main__':
    app.run(debug=True, host='0.0.0.0', port=8080)
