plugins {
    application
    kotlin("jvm") version "1.8.0"
    id("com.github.johnrengelman.shadow") version "8.1.1"
}

application {
    mainClass.set("com.example.crudapi.ApplicationKt")
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("io.ktor:ktor-server-netty:2.3.2")
    implementation("io.ktor:ktor-server-core:2.3.2")
    implementation("io.ktor:ktor-serialization-kotlinx-json:2.3.2")
    implementation("org.jetbrains.exposed:exposed-core:0.41.1")
    implementation("org.jetbrains.exposed:exposed-dao:0.41.1")
    implementation("org.jetbrains.exposed:exposed-jdbc:0.41.1")
    implementation("org.postgresql:postgresql:42.2.23")
    implementation("ch.qos.logback:logback-classic:1.2.3")
}

tasks.withType<com.github.johnrengelman.shadow.tasks.ShadowJar> {
    archiveFileName.set("app.jar")
    manifest {
        attributes(mapOf("Main-Class" to "com.example.crudapi.ApplicationKt"))
    }
}
