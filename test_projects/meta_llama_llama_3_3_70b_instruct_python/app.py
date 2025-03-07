
from flask import Flask, request, jsonify
import psycopg2
import os

app = Flask(__name__)

# Connect to the database
conn = psycopg2.connect(
    host="host.docker.internal",
    database="complang",
    user="testuser",
    password=os.environ['PGPASSWORD']
)

# Create a cursor object
cur = conn.cursor()

# Define the endpoints
@app.route('/users', methods=['POST'])
def create_user():
    data = request.get_json()
    cur.execute("INSERT INTO users (name, email) VALUES (%s, %s) RETURNING *", (data['name'], data['email']))
    user = cur.fetchone()
    conn.commit()
    return jsonify({'id': user[0], 'name': user[1], 'email': user[2]})

@app.route('/users', methods=['GET'])
def get_users():
    cur.execute("SELECT * FROM users")
    users = cur.fetchall()
    return jsonify([{'id': user[0], 'name': user[1], 'email': user[2]} for user in users])

@app.route('/users/<id>', methods=['GET'])
def get_user(id):
    cur.execute("SELECT * FROM users WHERE id = %s", (id,))
    user = cur.fetchone()
    if user:
        return jsonify({'id': user[0], 'name': user[1], 'email': user[2]})
    else:
        return jsonify({}), 404

@app.route('/users/<id>', methods=['PUT'])
def update_user(id):
    data = request.get_json()
    cur.execute("UPDATE users SET name = %s, email = %s WHERE id = %s RETURNING *", (data['name'], data['email'], id))
    user = cur.fetchone()
    if user:
        conn.commit()
        return jsonify({'id': user[0], 'name': user[1], 'email': user[2]})
    else:
        return jsonify({}), 404

@app.route('/users/<id>', methods=['DELETE'])
def delete_user(id):
    cur.execute("DELETE FROM users WHERE id = %s", (id,))
    if cur.rowcount > 0:
        conn.commit()
        return jsonify({}), 200
    else:
        return jsonify({}), 404

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8080)
