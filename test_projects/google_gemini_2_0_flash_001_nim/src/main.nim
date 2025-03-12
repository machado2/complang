import jester
import postgres
import dotenv
import json
import os

# Import the routes from the router module
import router

# Load environment variables from .env file
dotenv.load()

# Get the database password from the environment variables
let pgPassword = os.getenv("PGPASSWORD")
if pgPassword.isNilOrEmpty:
  quit("PGPASSWORD environment variable not set!")

# Define database connection string
let dbConnectionString = "host=host.docker.internal port=5432 dbname=complang user=testuser password=" & pgPassword

# Initialize the database connection pool
var dbPool = newPool(dbConnectionString, 5)

# Create the Jester app
var app = newJesterApp()

# Mount the routes
router.mount(app, dbPool)

# Start the server
app.listen(port = 8080)
