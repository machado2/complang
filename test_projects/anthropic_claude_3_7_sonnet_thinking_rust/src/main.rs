use actix_web::{web, App, HttpResponse, HttpServer, Responder, delete, get, post, put};
use serde::{Deserialize, Serialize};
use sqlx::{postgres::PgPoolOptions, Pool, Postgres};
use std::env;

#[derive(Debug, Serialize, Deserialize)]
struct User {
    id: i32,
    name: String,
    email: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct CreateUserRequest {
    name: String,
    email: String,
}

// Define a struct for our app state
struct AppState {
    db_pool: Pool<Postgres>,
}

// Handler for GET /users
#[get("/users")]
async fn get_users(data: web::Data<AppState>) -> impl Responder {
    match sqlx::query_as!(User, "SELECT id, name, email FROM users")
        .fetch_all(&data.db_pool)
        .await
    {
        Ok(users) => HttpResponse::Ok().json(users),
        Err(e) => {
            eprintln!("Failed to fetch users: {}", e);
            HttpResponse::InternalServerError().body("Failed to fetch users")
        }
    }
}

// Handler for GET /users/{id}
#[get("/users/{id}")]
async fn get_user(data: web::Data<AppState>, path: web::Path<i32>) -> impl Responder {
    let id = path.into_inner();

    match sqlx::query_as!(User, "SELECT id, name, email FROM users WHERE id = $1", id)
        .fetch_optional(&data.db_pool)
        .await
    {
        Ok(Some(user)) => HttpResponse::Ok().json(user),
        Ok(None) => HttpResponse::NotFound().body(format!("User with ID {} not found", id)),
        Err(e) => {
            eprintln!("Failed to fetch user: {}", e);
            HttpResponse::InternalServerError().body("Failed to fetch user")
        }
    }
}

// Handler for POST /users
#[post("/users")]
async fn create_user(data: web::Data<AppState>, user: web::Json<CreateUserRequest>) -> impl Responder {
    match sqlx::query_as!(
        User,
        "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email",
        user.name,
        user.email
    )
    .fetch_one(&data.db_pool)
    .await
    {
        Ok(user) => HttpResponse::Created().json(user),
        Err(e) => {
            eprintln!("Failed to create user: {}", e);
            HttpResponse::InternalServerError().body("Failed to create user")
        }
    }
}

// Handler for PUT /users/{id}
#[put("/users/{id}")]
async fn update_user(
    data: web::Data<AppState>,
    path: web::Path<i32>,
    user: web::Json<CreateUserRequest>,
) -> impl Responder {
    let id = path.into_inner();

    match sqlx::query!(
        "UPDATE users SET name = $1, email = $2 WHERE id = $3 RETURNING id",
        user.name,
        user.email,
        id
    )
    .fetch_optional(&data.db_pool)
    .await
    {
        Ok(Some(_)) => HttpResponse::Ok().finish(),
        Ok(None) => HttpResponse::NotFound().body(format!("User with ID {} not found", id)),
        Err(e) => {
            eprintln!("Failed to update user: {}", e);
            HttpResponse::InternalServerError().body("Failed to update user")
        }
    }
}

// Handler for DELETE /users/{id}
#[delete("/users/{id}")]
async fn delete_user(data: web::Data<AppState>, path: web::Path<i32>) -> impl Responder {
    let id = path.into_inner();

    match sqlx::query!("DELETE FROM users WHERE id = $1 RETURNING id", id)
        .fetch_optional(&data.db_pool)
        .await
    {
        Ok(Some(_)) => HttpResponse::Ok().finish(),
        Ok(None) => HttpResponse::NotFound().body(format!("User with ID {} not found", id)),
        Err(e) => {
            eprintln!("Failed to delete user: {}", e);
            HttpResponse::InternalServerError().body("Failed to delete user")
        }
    }
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    env_logger::init();

    // Get the password from environment variable
    let pg_password = env::var("PGPASSWORD").expect("PGPASSWORD environment variable must be set");
    
    // Create the database connection URL
    let database_url = format!(
        "postgres://testuser:{}@host.docker.internal:5432/complang",
        pg_password
    );

    // Create a connection pool
    let pool = PgPoolOptions::new()
        .max_connections(5)
        .connect(&database_url)
        .await
        .expect("Failed to create pool");

    println!("Starting server at http://0.0.0.0:8080");
    
    // Start the server
    HttpServer::new(move || {
        App::new()
            .app_data(web::Data::new(AppState {
                db_pool: pool.clone(),
            }))
            .service(get_users)
            .service(get_user)
            .service(create_user)
            .service(update_user)
            .service(delete_user)
    })
    .bind("0.0.0.0:8080")?
    .run()
    .await
}
