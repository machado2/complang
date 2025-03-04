
package com.example;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.jdbc.core.JdbcTemplate;

import javax.sql.DataSource;
import org.springframework.jdbc.datasource.DriverManagerDataSource;

@SpringBootApplication
public class App {
    public static void main(String[] args) {
        SpringApplication.run(App.class, args);
    }

    @Value("${spring.datasource.password}")
    private String dbPassword;

    @Bean
    public DataSource dataSource() {
        DriverManagerDataSource dataSource = new DriverManagerDataSource();
        dataSource.setDriverClassName("org.postgresql.Driver");
        dataSource.setUrl("jdbc:postgresql://host.docker.internal:5432/test_google_gemini_2_0_flash_001_java");
        dataSource.setUsername("postgres");
        dataSource.setPassword(dbPassword);
        return dataSource;
    }

    @Bean
    CommandLineRunner runner(DataSource dataSource) {
        return args -> {
            JdbcTemplate jdbcTemplate = new JdbcTemplate(dataSource);
            try {
                jdbcTemplate.execute("SELECT 1 FROM users LIMIT 1");
            } catch (Exception e) {
                System.out.println("Table 'users' does not exist, creating it...");
                jdbcTemplate.execute("""
                    CREATE TABLE users (
                        id SERIAL PRIMARY KEY,
                        name TEXT,
                        email TEXT
                    )
                """);
            }
        };
    }
}
