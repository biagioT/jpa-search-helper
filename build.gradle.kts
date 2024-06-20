buildscript {
    repositories {
        mavenCentral()
    }
    dependencies {
        classpath("com.amazonaws:aws-java-sdk-core:1.12.731")
    }
}

plugins {
    id("java-library")
    id("maven-publish")
    id("signing")
    id("org.springframework.boot") version "2.7.11"
    id("io.spring.dependency-management") version "1.0.11.RELEASE"
}

group = "app.gisgro"

val libVersion = "2.0.0"
version = libVersion

java.apply {
    sourceCompatibility = JavaVersion.VERSION_17
    targetCompatibility = JavaVersion.VERSION_17
    withSourcesJar()
    withJavadocJar()
}

repositories {
    mavenCentral()
}

tasks.bootJar {enabled = false}
tasks.jar {enabled = true}

dependencies {
    // JPA
    implementation("jakarta.persistence:jakarta.persistence-api:2.2.3")

    // Spring
    implementation("org.springframework.data:spring-data-jpa:2.7.11")
    implementation("org.springframework:spring-beans")

    // Lombok
    compileOnly("org.projectlombok:lombok:1.18.26")
    annotationProcessor("org.projectlombok:lombok:1.18.26")
    testImplementation("org.projectlombok:lombok:1.18.26")
    testCompileOnly("org.projectlombok:lombok:1.18.26")
    testAnnotationProcessor("org.projectlombok:lombok:1.18.26")
    implementation("com.fasterxml.jackson.datatype:jackson-datatype-jsr310:2.14.1")
    testImplementation("com.fasterxml.jackson.datatype:jackson-datatype-jsr310:2.14.1")
    // Utils
    implementation("org.apache.commons:commons-lang3:3.12.0")

    // Test
    testImplementation(platform("org.junit:junit-bom:5.10.2"))
    testImplementation("org.junit.jupiter:junit-jupiter")

    testImplementation("org.springframework.boot:spring-boot-starter-test")
    testImplementation("org.springframework.boot:spring-boot-starter-data-jpa")
    testImplementation("com.h2database:h2")
    //testImplementation "org.hamcrest:hamcrest-library:2.2"
    //testImplementation "org.mockito:mockito-core:3.12.4"
    //testImplementation "junit:junit:4.13.2"
}

publishing {
    publications {
        create<MavenPublication>("maven") {

            group = "app.gisgro"
            artifactId = "jpa-search-helper"
            version = libVersion
            from(components["java"])
            pom {
                name = "JPA Search Helper"
                description = "Helper library for building advanced and dynamic queries using JPA in Spring"

                licenses {
                    license {
                        name = "Mozilla Public License 2.0"
                        url = "https://www.mozilla.org/en-US/MPL/2.0/"
                    }
                }

                scm {
                    connection = "scm:git:git://github.com:VRTFinland/jpa-search-helper.git"
                    developerConnection = "scm:git:ssh://github.com:VRTFinland/jpa-search-helper.git"
                    url = "https://github.com/VRTFinland/jpa-search-helper/tree/main"
                }
            }
        }
    }
}
tasks {
    test {
        useJUnitPlatform()
    }
}