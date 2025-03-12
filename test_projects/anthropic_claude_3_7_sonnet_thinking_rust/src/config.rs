
use serde::Deserialize;
use std::env;

#[derive(Debug, Deserialize)]
pub struct Config {
    pub pg: deadpool_postgres::Config,
    pub server_addr: String,
}

impl Config {
    pub fn from_env() -> Result<Self, config::ConfigError> {
        let mut cfg = config::Config::new();
        cfg.set_default("server_addr", "0.0.0.0:8080")?;
        
        let pg_host = env::var("PG_HOST").unwrap_or_else(|_| "host.docker.internal".to_string());
        let pg_user = env::var("PG_USER").unwrap_or_else(|_| "testuser".to_string());
        let pg_dbname = env::var("PG_DBNAME").unwrap_or_else(|_| "complang".to_string());
        let pg_password = env::var("PGPASSWORD").expect("PGPASSWORD must be set");
        
        let mut pg_config = deadpool_postgres::Config::new();
        pg_config.host = Some(pg_host);
        pg_config.user = Some(pg_user);
        pg_config.dbname = Some(pg_dbname);
        pg_config.password = Some(pg_password);

        cfg.set_default("pg", pg_config)?;
        
        cfg.try_into()
    }
}
