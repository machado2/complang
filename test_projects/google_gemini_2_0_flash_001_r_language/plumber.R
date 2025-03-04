library(plumber)
library(RPostgres)
library(DBI)
library(jsonlite)

#* @apiTitle User Management API

# Database connection parameters
db_host <- "host.docker.internal"
db_port <- 5432
db_name <- "test_google_gemini_2_0_flash_001_r_language"
db_user <- "postgres"
db_password <- Sys.getenv("PGPASSWORD")

# Function to connect to the database
connect_db <- function() {
  tryCatch({
    conn <- dbConnect(RPostgres::Postgres(),
                      host = db_host,
                      port = db_port,
                      dbname = db_name,
                      user = db_user,
                      password = db_password)
    return(conn)
  }, error = function(e) {
    print(paste("Database connection error:", e$message))
    stop("Failed to connect to database")
    return(NULL)
  })
}

# Function to initialize the database if the users table doesn't exist
initialize_db <- function(conn) {
  if (!DBI::dbExistsTable(conn, "users")) {
    query <- "CREATE TABLE users (
      id SERIAL PRIMARY KEY,
      name TEXT,
      email TEXT
    )"
    dbExecute(conn, query)
  }
}

# Connect to the database and initialize it
conn <- connect_db()
initialize_db(conn)

#* @post /users
#* Create a new user
#* @param name The user's name
#* @param email The user's email
#* @serializer unboxedJSON
function(name, email) {
  query <- "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email"
  result <- dbGetQuery(conn, sqlInterpolate(ANSI(), query, name = name, email = email))
    dbCommit(conn)
  return(result)
}

#* @get /users
#* Get all users
#* @serializer unboxedJSON
function() {
  query <- "SELECT id, name, email FROM users"
  result <- dbGetQuery(conn, query)
  return(result)
}

#* @get /users/<id>
#* Get a user by ID
#* @param id The user ID
#* @serializer unboxedJSON
function(id) {
    id <- as.integer(id)
  query <- "SELECT id, name, email FROM users WHERE id = $1"
  result <- dbGetQuery(conn, sqlInterpolate(ANSI(), query, id = id))
  if (nrow(result) == 0) {
    response$status <- 404
    return(list(error = "User not found"))
  }
  return(result)
}

#* @put /users/<id>
#* Update a user by ID
#* @param id The user ID
#* @param name The user's name
#* @param email The user's email
function(id, name, email) {
    id <- as.integer(id)
  query <- "UPDATE users SET name = $2, email = $3 WHERE id = $1"
  result <- dbExecute(conn, sqlInterpolate(ANSI(), query, id = id, name = name, email = email))
  if (result == 0) {
    response$status <- 404
    return(list(error = "User not found"))
  }
    dbCommit(conn)
  return(NULL)
}

#* @delete /users/<id>
#* Delete a user by ID
#* @param id The user ID
function(id) {
    id <- as.integer(id)
  query <- "DELETE FROM users WHERE id = $1"
  result <- dbExecute(conn, sqlInterpolate(ANSI(), query, id = id))
  if (result == 0) {
    response$status <- 404
    return(list(error = "User not found"))
  }
    dbCommit(conn)
  return(NULL)
}
