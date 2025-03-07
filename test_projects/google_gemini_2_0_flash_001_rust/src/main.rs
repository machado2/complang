
use actix_web::{delete, get, post, put, web, App, HttpResponse, HttpServer, Responder, http::header};
use sqlx::{PgPool, FromRow};
use serde::{Deserialize, Serialize};
use dotenv::dotenv;
use std::env;
use log::{info, error};
use actix_web_httpauth::middleware::HttpAuthentication;
use actix_web_httpauth::extractors::bearer::BearerAuth;

#[derive(Debug, Deserialize)]
struct AppConfig {
    database_url: String,
    port: u16,
}

impl AppConfig {
    pub fn from_env() -> Result<Self, envy::Error> {
        envy::from_env()
    }
}

#[derive(Debug, Serialize, FromRow)]
struct User {
    id: i32,
    name: String,
    email: String,
}

#[derive(Debug, Deserialize)]
struct CreateUser {
    name: String,
    email: String,
}

#[derive(Debug, Deserialize)]
struct UpdateUser {
    name: String,
    email: String,
}


async fn validator(auth: BearerAuth) -> Result<bool, actix_web::Error> {
    if auth.token() == "my-secret-token" {
        Ok(true)
    } else {
        Err(actix_web::error::ErrorUnauthorized("invalid token"))
    }
}

#[post("/users")]
async fn create_user(db_pool: web::Data<PgPool>, user: web::Json<CreateUser>) -> impl Responder {
    let mut tx = match db_pool.begin().await {
        Ok(tx) => tx,
        Err(e) => {
            error!("Failed to begin transaction: {}", e);
            return HttpResponse::InternalServerError().finish();
        }
    };

    let result = sqlx::query!(
        "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id",
        user.name,
        user.email
    )
    .fetch_one(&mut *tx)
    .await;

    let user_id = match result {
        Ok(row) => row.id,
        Err(e) => {
            error!("Failed to insert user: {}", e);
            tx.rollback().await.ok();
            return HttpResponse::InternalServerError().finish();
        }
    };
    
    let new_user = User {
        id: user_id,
        name: user.name.clone(),
        email: user.email.clone(),
    };
    
    tx.commit().await.ok();

    HttpResponse::Created().header(header::CONTENT_TYPE, "application/json").json(new_user)
}

#[get("/users")]
async fn get_all_users(db_pool: web::Data<PgPool>) -> impl Responder {
    let users = match sqlx::query_as::<_, User>("SELECT id, name, email FROM users")
        .fetch_all(db_pool.get_ref())
        .await
    {
        Ok(users) => users,
        Err(e) => {
            error!("Failed to fetch users: {}", e);
            return HttpResponse::InternalServerError().finish();
        }
    };

    HttpResponse::Ok().header(header::CONTENT_TYPE, "application/json").json(users)
}

#[get("/users/{id}")]
async fn get_user(db_pool: web::Data<PgPool>, id: web::Path<i32>) -> impl Responder {
    let user_id = id.into_inner();
    let user = match sqlx::query_as::<_, User>("SELECT id, name, email FROM users WHERE id = $1")
        .bind(user_id)
        .fetch_optional(db_pool.get_ref())
        .await
    {
        Ok(Some(user)) => user,
        Ok(None) => return HttpResponse::NotFound().finish(),
        Err(e) => {
            error!("Failed to fetch user: {}", e);
            return HttpResponse::InternalServerError().finish();
        }
    };

    HttpResponse::Ok().header(header::CONTENT_TYPE, "application/json").json(user)
}

#[put("/users/{id}")]
async fn update_user(db_pool: web::Data<PgPool>, id: web::Path<i32>, user: web::Json<UpdateUser>) -> impl Responder {
    let user_id = id.into_inner();

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
            if result.rows_affected() == 0 {
                return HttpResponse::NotFound().finish();
            }
            HttpResponse::NoContent().finish()
        }
        Err(e) => {
            error!("Failed to update user: {}", e);
            HttpResponse::InternalServerError().finish()
        }
    }
}

#[delete("/users/{id}")]
async fn delete_user(db_pool: web::Data<PgPool>, id: web::Path<i32>) -> impl Responder {
    let user_id = id.into_inner();

    let result = sqlx::query!("DELETE FROM users WHERE id = $1", user_id)
        .execute(db_pool.get_ref())
        .await;

    match result {
        Ok(result) => {
            if result.rows_affected() == 0 {
                return HttpResponse::NotFound().finish();
            }
            HttpResponse::NoContent().finish()
        }
        Err(e) => {
            error!("Failed to delete user: {}", e);
            HttpResponse::InternalServerError().finish()
        }
    }
}

#[tokio::main]
async fn main() -> Result<(), sqlx::Error> {
    dotenv().ok();
    env_logger::init_from_env(env_logger::Env::default().default_filter_or("info"));

    let config = match AppConfig::from_env() {
        Ok(config) => config,
        Err(e) => {
            eprintln!("Failed to read configuration from the environment: {}", e);
            return Ok(());
        }
    };

    let db_pool = PgPool::connect(&config.database_url).await?;

    sqlx::migrate!("./migrations").run(&db_pool).await?;

    let auth = HttpAuthentication::bearer(validator);

    let port = config.port;
    info!("Starting server on port: {}", port);

    HttpServer::new(move || {
        App::new()
            .app_data(web::Data::new(db_pool.clone()))
            .wrap(auth.clone())
            .service(create_user)
            .service(get_all_users)
            .service(get_user)
            .service(update_user)
            .service(delete_user)
    })
    .bind(("0.0.0.0", port))?
    .run()
    .await?;

    Ok(())
}
