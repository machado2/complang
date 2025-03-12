package main

import (
    "database/sql"
    "encoding/json"
    "fmt"
    "log"
    "net/http"
    "os"
    "strconv"

    "github.com/gorilla/mux"
    _ "github.com/lib/pq"
)

type User struct {
    ID    int    ` + "`json:"id"`" + `
    Name  string ` + "`json:"name"`" + `
    Email string ` + "`json:"email"`" + `
}

var db *sql.DB

func main() {
    var err error
    password := os.Getenv("PGPASSWORD")
    if password == "" {
        log.Fatal("PGPASSWORD environment variable not set")
    }
    connStr := fmt.Sprintf("host=host.docker.internal port=5432 user=testuser password=%s dbname=complang sslmode=disable", password)
    db, err = sql.Open("postgres", connStr)
    if err != nil {
        log.Fatal(err)
    }
    defer db.Close()

    if err = db.Ping(); err != nil {
        log.Fatal(err)
    }

    r := mux.NewRouter()
    // POST /users: Create a new user
    r.HandleFunc("/users", createUser).Methods("POST")
    // GET /users: Get all users
    r.HandleFunc("/users", getUsers).Methods("GET")
    // GET /users/{id}: Get a specific user by id
    r.HandleFunc("/users/{id}", getUser).Methods("GET")
    // PUT /users/{id}: Update a user
    r.HandleFunc("/users/{id}", updateUser).Methods("PUT")
    // DELETE /users/{id}: Delete a user
    r.HandleFunc("/users/{id}", deleteUser).Methods("DELETE")

    fmt.Println("Server is running on port 8080")
    log.Fatal(http.ListenAndServe(":8080", r))
}

func createUser(w http.ResponseWriter, r *http.Request) {
    var user User
    if err := json.NewDecoder(r.Body).Decode(&user); err != nil {
        http.Error(w, "Invalid input", http.StatusBadRequest)
        return
    }
    var id int
    err := db.QueryRow("INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id", user.Name, user.Email).Scan(&id)
    if err != nil {
        http.Error(w, "Database error", http.StatusInternalServerError)
        return
    }
    user.ID = id
    w.Header().Set("Content-Type", "application/json")
    w.WriteHeader(http.StatusCreated)
    json.NewEncoder(w).Encode(user)
}

func getUsers(w http.ResponseWriter, r *http.Request) {
    rows, err := db.Query("SELECT id, name, email FROM users")
    if err != nil {
        http.Error(w, "Database error", http.StatusInternalServerError)
        return
    }
    defer rows.Close()

    users := []User{}
    for rows.Next() {
        var user User
        if err := rows.Scan(&user.ID, &user.Name, &user.Email); err != nil {
            http.Error(w, "Database error", http.StatusInternalServerError)
            return
        }
        users = append(users, user)
    }
    w.Header().Set("Content-Type", "application/json")
    json.NewEncoder(w).Encode(users)
}

func getUser(w http.ResponseWriter, r *http.Request) {
    vars := mux.Vars(r)
    id, err := strconv.Atoi(vars["id"])
    if err != nil {
        http.Error(w, "Invalid user id", http.StatusBadRequest)
        return
    }
    var user User
    err = db.QueryRow("SELECT id, name, email FROM users WHERE id=$1", id).Scan(&user.ID, &user.Name, &user.Email)
    if err == sql.ErrNoRows {
        http.Error(w, "User not found", http.StatusNotFound)
        return
    } else if err != nil {
        http.Error(w, "Database error", http.StatusInternalServerError)
        return
    }
    w.Header().Set("Content-Type", "application/json")
    json.NewEncoder(w).Encode(user)
}

func updateUser(w http.ResponseWriter, r *http.Request) {
    vars := mux.Vars(r)
    id, err := strconv.Atoi(vars["id"])
    if err != nil {
        http.Error(w, "Invalid user id", http.StatusBadRequest)
        return
    }
    var user User
    if err := json.NewDecoder(r.Body).Decode(&user); err != nil {
        http.Error(w, "Invalid input", http.StatusBadRequest)
        return
    }
    result, err := db.Exec("UPDATE users SET name=$1, email=$2 WHERE id=$3", user.Name, user.Email, id)
    if err != nil {
        http.Error(w, "Database error", http.StatusInternalServerError)
        return
    }
    affected, err := result.RowsAffected()
    if err != nil {
        http.Error(w, "Database error", http.StatusInternalServerError)
        return
    }
    if affected == 0 {
        http.Error(w, "User not found", http.StatusNotFound)
        return
    }
    w.WriteHeader(http.StatusOK)
}

func deleteUser(w http.ResponseWriter, r *http.Request) {
    vars := mux.Vars(r)
    id, err := strconv.Atoi(vars["id"])
    if err != nil {
        http.Error(w, "Invalid user id", http.StatusBadRequest)
        return
    }
    result, err := db.Exec("DELETE FROM users WHERE id=$1", id)
    if err != nil {
        http.Error(w, "Database error", http.StatusInternalServerError)
        return
    }
    affected, err := result.RowsAffected()
    if err != nil {
        http.Error(w, "Database error", http.StatusInternalServerError)
        return
    }
    if affected == 0 {
        http.Error(w, "User not found", http.StatusNotFound)
        return
    }
    w.WriteHeader(http.StatusOK)
}
