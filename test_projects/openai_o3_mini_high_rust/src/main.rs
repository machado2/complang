use actix_web::{web, App, HttpServer, Responder, HttpResponse, Error};
use sqlx::postgres::PgPoolOptions;
use serde::{Deserialize, Serialize};
use std::env;

#[derive(Debug, Serialize, sqlx::FromRow)]
struct User {
    id: i32,
    name: String,
    email: String,
}

#[derive(Debug, Deserialize)]
struct NewUser {
    name: String,
    email: String,
}

#[derive(Debug, Deserialize)]
struct UpdateUser {
    name: String,
    email: String,
}

async fn create_user(pool: web::Data<sqlx::PgPool>, new_user: web::Json<NewUser>) -> Result<impl Responder, Error> {
    let user = sqlx::query_as!(
        User,
        "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email",
        new_user.name,
        new_user.email
    )
    .fetch_one(pool.get_ref())
    .await
    .map_err(actix_web::error::ErrorInternalServerError)?;
    Ok(HttpResponse::Created().json(user))
}

async fn get_users(pool: web::Data<sqlx::PgPool>) -> Result<impl Responder, Error> {
    let users = sqlx::query_as!(
        User,
        "SELECT id, name, email FROM users"
    )
    .fetch_all(pool.get_ref())
    .await
    .map_err(actix_web::error::ErrorInternalServerError)?;
    Ok(HttpResponse::Ok().json(users))
}

async fn get_user(pool: web::Data<sqlx::PgPool>, user_id: web::Path<i32>) -> Result<impl Responder, Error> {
    let user = sqlx::query_as!(
        User,
        "SELECT id, name, email FROM users WHERE id = $1",
        *user_id
    )
    .fetch_optional(pool.get_ref())
    .await
    .map_err(actix_web::error::ErrorInternalServerError)?;
    if let Some(u) = user {
        Ok(HttpResponse::Ok().json(u))
    } else {
        Ok(HttpResponse::NotFound().finish())
    }
}

async fn update_user(pool: web::Data<sqlx::PgPool>, user_id: web::Path<i32>, update: web::Json<UpdateUser>) -> Result<impl Responder, Error> {
    let updated_user = sqlx::query_as!(
        User,
        "UPDATE users SET name = $1, email = $2 WHERE id = $3 RETURNING id, name, email",
        update.name,
        update.email,
        *user_id
    )
    .fetch_optional(pool.get_ref())
    .await
    .map_err(actix_web::error::ErrorInternalServerError)?;
    
    if let Some(user) = updated_user {
        Ok(HttpResponse::Ok().json(user))
    } else {
        Ok(HttpResponse::NotFound().finish())
    }
}

async fn delete_user(pool: web::Data<sqlx::PgPool>, user_id: web::Path<i32>) -> Result<impl Responder, Error> {
    let result = sqlx::query!(
        "DELETE FROM users WHERE id = $1",
        *user_id
    )
    .execute(pool.get_ref())
    .await
    .map_err(actix_web::error::ErrorInternalServerError)?;
    
    if result.rows_affected() == 0 {
        Ok(HttpResponse::NotFound().finish())
    } else {
        Ok(HttpResponse::NoContent().finish())
    }
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    env_logger::init();
    dotenv::dotenv().ok();
    let pg_password = env::var("PGPASSWORD").expect("PGPASSWORD must be set");
    let database_url = format!("postgres://testuser:{}@host.docker.internal:5432/complang", pg_password);
    
    let pool = PgPoolOptions::new()
        .max_connections(5)
        .connect(&database_url)
        .await
        .expect("Failed to connect to database.");

    HttpServer::new(move || {
        App::new()
            .app_data(web::Data::new(pool.clone()))
            .service(
                web::scope("/users")
                    .route("", web::post().to(create_user))
                    .route("", web::get().to(get_users))
                    .route("/{id}", web::get().to(get_user))
                    .route("/{id}", web::put().to(update_user))
                    .route("/{id}", web::delete().to(delete_user))
            )
    })
    .bind("0.0.0.0:8080")?
    .run()
    .await
}
