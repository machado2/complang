
plugins {
    kotlin("jvm") version "1.8.10"
    kotlin("plugin.serialization") version "1.8.10"
    application
}

group = "com.example"
version = "0.0.1"

application {
    mainClass.set("com.example.ApplicationKt")
}

repositories {
    mavenCentral()
}

dependencies {
    // Kotlin
    implementation("org.jetbrains.kotlin:kotlin-stdlib")
    
    // Ktor server
    implementation("io.ktor:ktor-server-core:2.2.4")
    implementation("io.ktor:ktor-server-netty:2.2.4")
    implementation("io.ktor:ktor-server-content-negotiation:2.2.4")
    implementation("io.ktor:ktor-serialization-jackson:2.2.4")
    implementation("io.ktor:ktor-server-status-pages:2.2.4")
    
    // Database
    implementation("org.jetbrains.exposed:exposed-core:0.41.1")
    implementation("org.jetbrains.exposed:exposed-dao:0.41.1")
    implementation("org.jetbrains.exposed:exposed-jdbc:0.41.1")
    implementation("org.postgresql:postgresql:42.5.4")
    implementation("com.zaxxer:HikariCP:5.0.1")
    
    // Logging
    implementation("ch.qos.logback:logback-classic:1.4.6")
    
    // Testing
    testImplementation("io.ktor:ktor-server-test-host:2.2.4")
    testImplementation("org.jetbrains.kotlin:kotlin-test")
}

tasks.jar {
    manifest {
        attributes["Main-Class"] = "com.example.ApplicationKt"
    }
    
    // Include all dependencies in the jar
    from(configurations.runtimeClasspath.get().map { if (it.isDirectory) it else zipTree(it) })
    duplicatesStrategy = DuplicatesStrategy.EXCLUDE
}
