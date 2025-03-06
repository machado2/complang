# src/main.rs

use actix_web::{web, App, HttpResponse, HttpServer, Responder, Result, middleware};
use serde::{Deserialize, Serialize};
use sqlx::PgPool;
use std::env;
use dotenvy::dotenv;
use log::info;
use tokio::runtime::Runtime;

#[derive(Debug, Serialize, Deserialize)]
pub struct User {
    pub id: i32,
    pub name: String,
    pub email: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct CreateUser {
    pub name: String,
    pub email: String,
}

async fn create_user(
    db_pool: web::Data<PgPool>,
    user_data: web::Json<CreateUser>,
) -> Result<HttpResponse> {
    info!("Creating user with data: {:?}", user_data);
    let user = user_data.into_inner();

    let result = sqlx::query_as!(
        User,
        "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email",
        user.name,
        user.email
    )
    .fetch_one(db_pool.get_ref())
    .await;

    match result {
        Ok(user) => {
            info!("User created successfully with id: {}", user.id);
            Ok(HttpResponse::Created().json(user))
        }
        Err(e) => {
            eprintln!("Error creating user: {}", e);
            Err(actix_web::error::ErrorInternalServerError(e))
        }
    }
}

async fn get_users(db_pool: web::Data<PgPool>) -> Result<HttpResponse> {
    info!("Getting all users");
    let result = sqlx::query_as!(User, "SELECT id, name, email FROM users")
        .fetch_all(db_pool.get_ref())
        .await;

    match result {
        Ok(users) => {
            info!("Successfully retrieved {} users", users.len());
            Ok(HttpResponse::Ok().json(users))
        }
        Err(e) => {
            eprintln!("Error getting users: {}", e);
            Err(actix_web::error::ErrorInternalServerError(e))
        }
    }
}

async fn get_user(db_pool: web::Data<PgPool>, id: web::Path<i32>) -> Result<HttpResponse> {
    info!("Getting user with id: {}", id);
    let user_id = id.into_inner();

    let result = sqlx::query_as!(User, "SELECT id, name, email FROM users WHERE id = $1", user_id)
        .fetch_optional(db_pool.get_ref())
        .await;

    match result {
        Ok(user) => match user {
            Some(user) => {
                info!("User found with id: {}", user.id);
                Ok(HttpResponse::Ok().json(user))
            }
            None => {
                info!("User not found with id: {}", user_id);
                Ok(HttpResponse::NotFound().finish())
            }
        },
        Err(e) => {
            eprintln!("Error getting user: {}", e);
            Err(actix_web::error::ErrorInternalServerError(e))
        }
    }
}

async fn update_user(
    db_pool: web::Data<PgPool>,
    id: web::Path<i32>,
    user_data: web::Json<CreateUser>,
) -> Result<HttpResponse> {
    info!("Updating user with id: {} and data: {:?}", id, user_data);
    let user_id = id.into_inner();
    let user = user_data.into_inner();

    let result = sqlx::query!(
        "UPDATE users SET name = $1, email = $2 WHERE id = $3",
        user.name,
        user.email,
        user_id
    )
    .execute(db_pool.get_ref())
    .await;

    match result {
        Ok(result) => {
            if result.rows_affected() > 0 {
                info!("User updated successfully with id: {}", user_id);
                Ok(HttpResponse::NoContent().finish())
            } else {
                info!("User not found with id: {}", user_id);
                Ok(HttpResponse::NotFound().finish())
            }
        }
        Err(e) => {
            eprintln!("Error updating user: {}", e);
            Err(actix_web::error::ErrorInternalServerError(e))
        }
    }
}

async fn delete_user(db_pool: web::Data<PgPool>, id: web::Path<i32>) -> Result<HttpResponse> {
    info!("Deleting user with id: {}", id);
    let user_id = id.into_inner();

    let result = sqlx::query!("DELETE FROM users WHERE id = $1", user_id)
        .execute(db_pool.get_ref())
        .await;

    match result {
        Ok(result) => {
            if result.rows_affected() > 0 {
                info!("User deleted successfully with id: {}", user_id);
                Ok(HttpResponse::NoContent().finish())
            } else {
                info!("User not found with id: {}", user_id);
                Ok(HttpResponse::NotFound().finish())
            }
        }
        Err(e) => {
            eprintln!("Error deleting user: {}", e);
            Err(actix_web::error::ErrorInternalServerError(e))
        }
    }
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    dotenv().ok();
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    let db_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    let db_pool = PgPool::connect(&db_url).await.expect("Failed to create pool.");

    info!("Starting server");

    HttpServer::new(move || {
        App::new()
            .app_data(web::Data::new(db_pool.clone()))
            .wrap(middleware::Logger::default())
            .service(
                web::resource("/users")
                    .route(web::post().to(create_user))
                    .route(web::get().to(get_users)),
            )
            .service(
                web::resource("/users/{id}")
                    .route(web::get().to(get_user))
                    .route(web::put().to(update_user))
                    .route(web::delete().to(delete_user)),
            )
    })
    .bind("0.0.0.0:8080")?
    .run()
    .await
}
