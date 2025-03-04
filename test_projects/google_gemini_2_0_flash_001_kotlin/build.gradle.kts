
plugins {
    kotlin("jvm") version "1.9.21"
    id("io.ktor.plugin") version "2.3.7"
    id("org.jetbrains.kotlin.plugin.serialization") version "1.9.21"
}

group = "com.example"
version = "0.0.1-1741202685"

repositories {
    mavenCentral()
}

dependencies {
    implementation("io.ktor:ktor-server-core-jvm:2.3.7")
    implementation("io.ktor:ktor-server-netty-jvm:2.3.7")
    implementation("ch.qos.logback:logback-classic:1.2.3")
    implementation("io.ktor:ktor-server-content-negotiation-jvm:2.3.7")
    implementation("io.ktor:ktor-serialization-kotlinx-json-jvm:2.3.7")
    implementation("org.postgresql:postgresql:42.6.0") // PostgreSQL driver
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.6.2")

    testImplementation("io.ktor:ktor-server-tests-jvm:2.3.7")
    testImplementation("org.jetbrains.kotlin:kotlin-test-junit:1.9.21")
}

kotlin {
    jvmToolchain(11)
}

ktor {
    fatJar {
        archiveFileName.set("app.jar")
    }
}
