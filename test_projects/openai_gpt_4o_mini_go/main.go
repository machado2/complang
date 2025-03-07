
package main

import (
    "database/sql"
    "encoding/json"
    "net/http"
    "os"
    _ "github.com/lib/pq"
)

type User struct {
    ID    int    `json:"id"`
    Name  string `json:"name"`
    Email string `json:"email"`
}

var db *sql.DB

func init() {
    var err error
    connStr := "postgres://testuser:" + os.Getenv("PGPASSWORD") + "@host.docker.internal:5432/complang?sslmode=disable"
    db, err = sql.Open("postgres", connStr)
    if err != nil {
        panic(err)
    }
}

func createUser(w http.ResponseWriter, r *http.Request) {
    var user User
    if err := json.NewDecoder(r.Body).Decode(&user); err != nil {
        http.Error(w, err.Error(), http.StatusBadRequest)
        return
    }
    err := db.QueryRow("INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id", user.Name, user.Email).Scan(&user.ID)
    if err != nil {
        http.Error(w, err.Error(), http.StatusInternalServerError)
        return
    }
    w.WriteHeader(http.StatusCreated)
    json.NewEncoder(w).Encode(user)
}

func getUsers(w http.ResponseWriter, r *http.Request) {
    rows, err := db.Query("SELECT id, name, email FROM users")
    if err != nil {
        http.Error(w, err.Error(), http.StatusInternalServerError)
        return
    }
    defer rows.Close()

    var users []User
    for rows.Next() {
        var user User
        if err := rows.Scan(&user.ID, &user.Name, &user.Email); err != nil {
            http.Error(w, err.Error(), http.StatusInternalServerError)
            return
        }
        users = append(users, user)
    }
    json.NewEncoder(w).Encode(users)
}

func getUser(w http.ResponseWriter, r *http.Request) {
    id := r.URL.Path[len("/users/"):]

    var user User
    err := db.QueryRow("SELECT id, name, email FROM users WHERE id = $1", id).Scan(&user.ID, &user.Name, &user.Email)
    if err != nil {
        if err == sql.ErrNoRows {
            http.NotFound(w, r)
            return
        }
        http.Error(w, err.Error(), http.StatusInternalServerError)
        return
    }
    json.NewEncoder(w).Encode(user)
}

func updateUser(w http.ResponseWriter, r *http.Request) {
    id := r.URL.Path[len("/users/"):]

    var user User
    if err := json.NewDecoder(r.Body).Decode(&user); err != nil {
        http.Error(w, err.Error(), http.StatusBadRequest)
        return
    }

    res, err := db.Exec("UPDATE users SET name = $1, email = $2 WHERE id = $3", user.Name, user.Email, id)
    if err != nil || res.RowsAffected() == 0 {
        http.Error(w, "User not found", http.StatusNotFound)
        return
    }

    w.WriteHeader(http.StatusNoContent)
}

func deleteUser(w http.ResponseWriter, r *http.Request) {
    id := r.URL.Path[len("/users/"):]

    res, err := db.Exec("DELETE FROM users WHERE id = $1", id)
    if err != nil || res.RowsAffected() == 0 {
        http.Error(w, "User not found", http.StatusNotFound)
        return
    }

    w.WriteHeader(http.StatusNoContent)
}

func main() {
    http.HandleFunc("/users", createUser)
    http.HandleFunc("/users/", func(w http.ResponseWriter, r *http.Request) {
        switch r.Method {
        case "GET":
            if len(r.URL.Path) > len("/users/") {
                getUser(w, r)
            } else {
                getUsers(w, r)
            }
        case "PUT":
            updateUser(w, r)
        case "DELETE":
            deleteUser(w, r)
        default:
            http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
        }
    })
    http.ListenAndServe(":8080", nil)
}
