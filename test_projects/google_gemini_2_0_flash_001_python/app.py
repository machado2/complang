
from flask import Flask, request, jsonify
import psycopg2
import os

app = Flask(__name__)

# Database configuration
DB_HOST = "host.docker.internal"
DB_PORT = 5432
DB_NAME = "complang"
DB_USER = "testuser"
DB_PASSWORD = os.environ.get("PGPASSWORD")

def get_db_connection():
    try:
        conn = psycopg2.connect(host=DB_HOST, port=DB_PORT, database=DB_NAME, user=DB_USER, password=DB_PASSWORD)
        return conn
    except psycopg2.Error as e:
        print(f"Database connection error: {e}")
        return None

# Create the users table if it doesn't exist (should already exist, but for robustness)
def setup_database():
    conn = get_db_connection()
    if conn:
        cur = conn.cursor()
        try:
            cur.execute("""
                CREATE TABLE IF NOT EXISTS users (
                    id SERIAL PRIMARY KEY,
                    name TEXT NOT NULL,
                    email TEXT NOT NULL
                );
            """)
            conn.commit()
            cur.close()
            conn.close()
        except psycopg2.Error as e:
            print(f"Error creating table: {e}")

setup_database()

@app.route('/users', methods=['POST'])
def create_user():
    data = request.get_json()
    name = data.get('name')
    email = data.get('email')

    if not name or not email:
        return jsonify({"message": "Name and email are required"}), 400

    conn = get_db_connection()
    if conn:
        cur = conn.cursor()
        try:
            cur.execute("INSERT INTO users (name, email) VALUES (%s, %s) RETURNING id, name, email", (name, email))
        except psycopg2.Error as e:
          print(f"Error inserting values: {e}")
          return jsonify({"message": "Error creating user."}), 500

        user = cur.fetchone()
        conn.commit()
        cur.close()
        conn.close()
        if user is not None:
          user_id, user_name, user_email = user
          return jsonify({"id": user_id, "name": user_name, "email": user_email}), 201
        else:
          return jsonify({"message": "Error creating user, no user returned."}), 500
    else:
        return jsonify({"message": "Database connection failed"}), 500

@app.route('/users', methods=['GET'])
def get_users():
    conn = get_db_connection()
    if conn:
        cur = conn.cursor()
        cur.execute("SELECT id, name, email FROM users")
        users = cur.fetchall()
        cur.close()
        conn.close()

        user_list = []
        for user in users:
            user_list.append({"id": user[0], "name": user[1], "email": user[2]})
        return jsonify(user_list), 200
    else:
        return jsonify({"message": "Database connection failed"}), 500

@app.route('/users/<int:user_id>', methods=['GET'])
def get_user(user_id):
    conn = get_db_connection()
    if conn:
        cur = conn.cursor()
        cur.execute("SELECT id, name, email FROM users WHERE id = %s", (user_id,))
        user = cur.fetchone()
        cur.close()
        conn.close()

        if user:
            return jsonify({"id": user[0], "name": user[1], "email": user[2]}), 200
        else:
            return jsonify({"message": "User not found"}), 404
    else:
        return jsonify({"message": "Database connection failed"}), 500

@app.route('/users/<int:user_id>', methods=['PUT'])
def update_user(user_id):
    data = request.get_json()
    name = data.get('name')
    email = data.get('email')

    if not name or not email:
        return jsonify({"message": "Name and email are required"}), 400

    conn = get_db_connection()
    if conn:
        cur = conn.cursor()
        cur.execute("UPDATE users SET name = %s, email = %s WHERE id = %s", (name, email, user_id))
        conn.commit()
        rows_affected = cur.rowcount
        cur.close()
        conn.close()

        if rows_affected > 0:
            return '', 204
        else:
            return jsonify({"message": "User not found"}), 404
    else:
        return jsonify({"message": "Database connection failed"}), 500

@app.route('/users/<int:user_id>', methods=['DELETE'])
def delete_user(user_id):
    conn = get_db_connection()
    if conn:
        cur = conn.cursor()
        cur.execute("DELETE FROM users WHERE id = %s", (user_id,))
        conn.commit()
        rows_affected = cur.rowcount
        cur.close()
        conn.close()

        if rows_affected > 0:
            return '', 204
        else:
            return jsonify({"message": "User not found"}), 404
    else:
        return jsonify({"message": "Database connection failed"}), 500

if __name__ == '__main__':
    app.run(debug=True, host='0.0.0.0', port=8080)
