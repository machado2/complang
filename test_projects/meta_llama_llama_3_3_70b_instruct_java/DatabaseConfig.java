
package com.example.complang;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.jdbc.DataSourceBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

import javax.sql.DataSource;
import java.sql.SQLException;

@Configuration
public class DatabaseConfig {
    @Value("${PGPASSWORD}")
    private String password;

    @Bean
    @Primary
    @ConfigurationProperties(prefix = "spring.datasource")
    public DataSource dataSource() {
        return DataSourceBuilder.create()
               .driverClassName("org.postgresql.Driver")
               .url("jdbc:postgresql://host.docker.internal:5432/complang")
               .username("testuser")
               .password(password)
               .build();
    }
}
