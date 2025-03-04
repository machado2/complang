use actix_web::{web, App, HttpResponse, HttpServer, Responder, Result, http::header};
use tokio_postgres::{Client, NoTls};
use std::env;
use dotenv::dotenv;
use serde::{Deserialize, Serialize};
use actix_web::http::StatusCode;
use log::{info, error};
use env_logger;
use actix_web::error::ResponseError;
use mime;
use std::fmt;
use std::error::Error;
use tokio;
use std::time::Duration;

#[derive(Debug, Deserialize, Serialize)]
struct User {
    id: i32,
    name: String,
    email: String,
}

#[derive(Debug, Deserialize, Serialize)]
struct CreateUser {
    name: String,
    email: String,
}

#[derive(Debug)]
struct MyError(tokio_postgres::Error);

impl fmt::Display for MyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Postgres error: {}", self.0)
    }
}

impl Error for MyError {}

impl ResponseError for MyError {}

impl From<tokio_postgres::Error> for MyError {
    fn from(err: tokio_postgres::Error) -> Self {
        MyError(err)
    }
}


async fn get_db_connection() -> Result<Client, MyError> {
    dotenv().ok();

    let db_url = env::var("DATABASE_URL")
        .expect("DATABASE_URL must be set");

    let mut attempts = 0;
    loop {
        match tokio_postgres::connect(&db_url, NoTls).await {
            Ok((client, connection)) => {
                tokio::spawn(async move {
                    if let Err(e) = connection.await {
                        eprintln!("connection error: {}", e);
                    }
                });
                return Ok(client);
            }
            Err(e) => {
                attempts += 1;
                if attempts > 5 {
                    return Err(MyError(e));
                }
                println!("Failed to connect to database, retrying in 2 seconds... (attempt {})", attempts);
                tokio::time::sleep(Duration::from_secs(2)).await;
            }
        }
    }
}


async fn create_user(user: web::Json<CreateUser>) -> Result<HttpResponse, MyError> {
    info!("Creating user: {:?}", user);
    let client = get_db_connection().await?;

    let name = &user.name;
    let email = &user.email;

    let row = client.query_one(
            "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email",
            &[&name, &email],
        ).await?;

    let user = User {
        id: row.get(0),
        name: row.get(1),
        email: row.get(2),
    };

    Ok(HttpResponse::Created()
        .insert_header(header::ContentType(mime::APPLICATION_JSON))
        .json(user))
}


async fn get_users() -> Result<HttpResponse, MyError> {
    info!("Getting all users");
    let client = get_db_connection().await?;

    let mut users: Vec<User> = Vec::new();

    for row in client.query("SELECT id, name, email FROM users", &[]).await? {
        users.push(User {
            id: row.get(0),
            name: row.get(1),
            email: row.get(2),
        });
    }

    Ok(HttpResponse::Ok()
        .insert_header(header::ContentType(mime::APPLICATION_JSON))
        .json(users))
}


async fn get_user(id: web::Path<i32>) -> Result<HttpResponse, MyError> {
    let user_id = id.into_inner();
    info!("Getting user with id: {}", user_id);
    let client = get_db_connection().await?;

    let result = client.query_opt(
        "SELECT id, name, email FROM users WHERE id = $1",
        &[&user_id],
    ).await?;

    match result {
        Some(row) => {
            let user = User {
                id: row.get(0),
                name: row.get(1),
                email: row.get(2),
            };
             Ok(HttpResponse::Ok()
                .insert_header(header::ContentType(mime::APPLICATION_JSON))
                .json(user))
        }
        None => {
            Ok(HttpResponse::NotFound().finish())
        }
    }
}


async fn update_user(id: web::Path<i32>, user: web::Json<CreateUser>) -> Result<HttpResponse, MyError> {
    let user_id = id.into_inner();
    info!("Updating user with id: {}", user_id);
    let client = get_db_connection().await?;

    let result = client.execute(
        "UPDATE users SET name = $1, email = $2 WHERE id = $3",
        &[&user.name, &user.email, &user_id.to_string()],
    ).await?;

    match result {
        1 => {
            Ok(HttpResponse::NoContent().finish())
        }
        _ => {
            Ok(HttpResponse::NotFound().finish())
        }
    }
}


async fn delete_user(id: web::Path<i32>) -> Result<HttpResponse, MyError> {
    let user_id = id.into_inner();
    info!("Deleting user with id: {}", user_id);
    let client = get_db_connection().await?;

    let result = client.execute(
        "DELETE FROM users WHERE id = $1",
        &[&user_id.to_string()],
    ).await?;

    match result {
        1 => {
            Ok(HttpResponse::NoContent().finish())
        }
        _ => {
            Ok(HttpResponse::NotFound().finish())
        }
    }
}

#[tokio::main]
async fn main() -> std::io::Result<()> {
    dotenv().ok();
    env_logger::init_from_env(env_logger::Env::default().default_filter_or("info"));


    let db_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    let client = get_db_connection().await.expect("Failed to connect to database");


    client.execute(
        "CREATE TABLE IF NOT EXISTS users (
            id SERIAL PRIMARY KEY,
            name TEXT NOT NULL,
            email TEXT NOT NULL
        )",
        &[],
    ).await.expect("Failed to create table");
    log::info!("Starting http server: 0.0.0.0:8080");


    HttpServer::new(move ||  {
        App::new()
            .route("/users", web::post().to(create_user))
            .route("/users", web::get().to(get_users))
            .route("/users/{id}", web::get().to(get_user))
            .route("/users/{id}", web::put().to(update_user))
            .route("/users/{id}", web::delete().to(delete_user))
    })
    .bind(("0.0.0.0", 8080))?
    .run()
    .await
}
