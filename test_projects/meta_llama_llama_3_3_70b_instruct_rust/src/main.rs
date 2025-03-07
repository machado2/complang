
use tokio_postgres::{NoTls, Row};
use warp::Filter;
use serde::{Serialize, Deserialize};

// Define the User struct
#[derive(Serialize, Deserialize)]
struct User {
    id: i32,
    name: String,
    email: String,
}

// Create a PostgreSQL connection pool
#[tokio::main]
async fn main() {
    let (client, connection) = tokio_postgres::connect(
        "host=host.docker.internal port=5432 dbname=complang user=testuser password=Saloon5-Moody-Observing",
        NoTls,
    ).await.expect("Failed to connect to database");

    // Create a warp filter for the API
    let api = warp::post()
      .and(warp::path("users"))
      .and(warp::body::json())
      .map(move |user: User| {
            // Create a new user
            let row = client.query_one("INSERT INTO users (name, email) VALUES ($1, $2) RETURNING *", &[&user.name, &user.email]).await.expect("Failed to create user");
            User {
                id: row.get("id"),
                name: row.get("name"),
                email: row.get("email"),
            }
        });

    // Create a warp filter for getting all users
    let get_users = warp::get()
      .and(warp::path("users"))
      .map(move || {
            // Get all users
            let rows = client.query("SELECT * FROM users", &[]).await.expect("Failed to get users");
            let users: Vec<User> = rows.into_iter().map(|row| User {
                id: row.get("id"),
                name: row.get("name"),
                email: row.get("email"),
            }).collect();
            warp::reply::json(&users)
        });

    // Create a warp filter for getting a user by id
    let get_user = warp::get()
      .and(warp::path("users"))
      .and(warp::path::param())
      .map(move |id: i32| {
            // Get a user by id
            let row = client.query_one("SELECT * FROM users WHERE id = $1", &[&id]).await.expect("Failed to get user");
            User {
                id: row.get("id"),
                name: row.get("name"),
                email: row.get("email"),
            }
        });

    // Create a warp filter for updating a user
    let update_user = warp::put()
      .and(warp::path("users"))
      .and(warp::path::param())
      .and(warp::body::json())
      .map(move |id: i32, user: User| {
            // Update a user
            client.execute("UPDATE users SET name = $1, email = $2 WHERE id = $3", &[&user.name, &user.email, &id]).await.expect("Failed to update user");
            warp::reply::json(&user)
        });

    // Create a warp filter for deleting a user
    let delete_user = warp::delete()
      .and(warp::path("users"))
      .and(warp::path::param())
      .map(move |id: i32| {
            // Delete a user
            client.execute("DELETE FROM users WHERE id = $1", &[&id]).await.expect("Failed to delete user");
            warp::reply::json(&())
        });

    // Run the warp server
    warp::serve(api.or(get_users).or(get_user).or(update_user).or(delete_user))
      .run(([127, 0, 0, 1], 8080))
      .await;
}

