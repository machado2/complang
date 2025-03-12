
use axum::{
    extract::{Path, State},
    http::StatusCode,
    response::IntoResponse,
    routing::{delete, get, post, put},
    Json,
    Router,
};
use dotenv::dotenv;
use serde::{Deserialize, Serialize};
use sqlx::{postgres::PgPoolOptions, Pool, Postgres};
use std::env;
use std::net::SocketAddr;
use tower_http::cors::CorsLayer;
use tracing::info;


#[derive(Clone)]
struct AppState {
    db_pool: Pool<Postgres>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct User {
    pub id: i32,
    pub name: String,
    pub email: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct CreateUser {
    pub name: String,
    pub email: String,
}

async fn create_user(
    State(state): State<AppState>,
    Json(payload): Json<CreateUser>,
) -> Result<(StatusCode, Json<User>), StatusCode> {
    let query = r#"INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email;"#;

    let result = sqlx::query_as::<_, User>(query)
        .bind(payload.name)
        .bind(payload.email)
        .fetch_one(&state.db_pool)
        .await;

    match result {
        Ok(user) => {
            info!("User created: {:?}", user);
            Ok((StatusCode::CREATED, Json(user)))
        }
        Err(err) => {
            eprintln!("Error creating user: {:?}", err);
            Err(StatusCode::INTERNAL_SERVER_ERROR)
        }
    }
}

async fn get_users(State(state): State<AppState>) -> Result<Json<Vec<User>>, StatusCode> {
    let query = "SELECT id, name, email FROM users";

    let result = sqlx::query_as::<_, User>(query).fetch_all(&state.db_pool).await;

    match result {
        Ok(users) => Ok(Json(users)),
        Err(err) => {
            eprintln!("Error getting users: {:?}", err);
            Err(StatusCode::INTERNAL_SERVER_ERROR)
        }
    }
}

async fn get_user(
    Path(id): Path<i32>,
    State(state): State<AppState>,
) -> Result<Json<User>, StatusCode> {
    let query = "SELECT id, name, email FROM users WHERE id = $1";

    let result = sqlx::query_as::<_, User>(query)
        .bind(id)
        .fetch_optional(&state.db_pool)
        .await;

    match result {
        Ok(Some(user)) => Ok(Json(user)),
        Ok(None) => Err(StatusCode::NOT_FOUND),
        Err(err) => {
            eprintln!("Error getting user: {:?}", err);
            Err(StatusCode::INTERNAL_SERVER_ERROR)
        }
    }
}

async fn update_user(
    Path(id): Path<i32>,
    State(state): State<AppState>,
    Json(payload): Json<CreateUser>,
) -> Result<StatusCode, StatusCode> {
    let query = "UPDATE users SET name = $1, email = $2 WHERE id = $3";

    let result = sqlx::query(query)
        .bind(payload.name)
        .bind(payload.email)
        .bind(id)
        .execute(&state.db_pool)
        .await;

    match result {
        Ok(result) => {
            if result.rows_affected() > 0 {
                Ok(StatusCode::NO_CONTENT)
            } else {
                Err(StatusCode::NOT_FOUND)
            }
        }
        Err(err) => {
            eprintln!("Error updating user: {:?}", err);
            Err(StatusCode::INTERNAL_SERVER_ERROR)
        }
    }
}

async fn delete_user(
    Path(id): Path<i32>,
    State(state): State<AppState>,
) -> Result<StatusCode, StatusCode> {
    let query = "DELETE FROM users WHERE id = $1";

    let result = sqlx::query(query).bind(id).execute(&state.db_pool).await;

    match result {
        Ok(result) => {
            if result.rows_affected() > 0 {
                Ok(StatusCode::NO_CONTENT)
            } else {
                Err(StatusCode::NOT_FOUND)
            }
        }
        Err(err) => {
            eprintln!("Error deleting user: {:?}", err);
            Err(StatusCode::INTERNAL_SERVER_ERROR)
        }
    }
}

#[tokio::main]
async fn main() -> Result<(), sqlx::Error> {
    dotenv().ok();
    tracing_subscriber::fmt::init();

    let db_url = format!(
        "postgres://{}:{}@{}:{}/{}",
        env::var("DB_USER").unwrap_or("testuser".to_string()),
        env::var("DB_PASSWORD").expect("DB_PASSWORD must be set"),
        env::var("DB_HOST").unwrap_or("host.docker.internal".to_string()),
        env::var("DB_PORT").unwrap_or("5432".to_string()),
        env::var("DB_NAME").unwrap_or("complang".to_string()),
    );

    let db_pool = PgPoolOptions::new()
        .max_connections(5)
        .connect(&db_url)
        .await
        .expect("Failed to create pool.");

    let state = AppState { db_pool };

    let app = Router::new()
        .route("/users", post(create_user).get(get_users))
        .route("/users/:id", get(get_user).put(update_user).delete(delete_user))
        .with_state(state)
        .layer(
            CorsLayer::new()
                .allow_origin(tower_http::cors::Any)
                .allow_methods(tower_http::cors::Any)
                .allow_headers(tower_http::cors::Any),
        );

    let addr = SocketAddr::from(([0, 0, 0, 0], 8080));
    info!("listening on {}", addr);
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .unwrap();

    Ok(())
}
