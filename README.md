# GamiLife Backend

[![Build](https://img.shields.io/badge/build-passing-lightgrey)](#)
![Version](https://img.shields.io/badge/version-0.7.0-blue)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

## About The Project

**GamiLife Backend** is a modular Spring Boot backend for a gamified productivity platform. It combines:

- personal task and habit management,
- collaborative group workflows,
- social interactions (group membership, invites, chat),
- gamification mechanics (levels, achievements, inventory, store),
- and supporting services (authentication, notifications, communication).

The codebase is organized as a **multi-module Maven project** with clear boundaries between:

- **business modules** (`modules/*`),
- **public inter-module contracts** (`api/*`),
- **cross-cutting shared modules** (`shared/*`),
- and the **application bootstrap module** (`app`).

Main entry point: `app/src/main/java/pl/gamilife/app/StartUp.java`.

## Key Features

- **Authentication & security**: registration/login, JWT access + refresh tokens, email verification, password reset, OAuth2 Google flow.
- **User management**: profile update and onboarding completion.
- **Task management**: personal tasks, habits, and task notifications/reminders.
- **Group collaboration**: groups, members, invitations, join requests, and group chat messages.
- **Group tasks**: assign and track collaborative tasks in groups.
- **Gamification**: levels, achievements, item slots, rarities, inventory, and statistics.
- **Group shop**: group-level shop configuration, items, and member-owned group items.
- **Pomodoro integration**: pomodoro task item endpoints.
- **Operational features**: actuator health endpoint, OpenAPI/Swagger docs (enabled in default config), graceful shutdown, and request rate limiting.

## Built With / Technologies

- **Java 21**
- **Maven (multi-module, with Maven Wrapper `./mvnw`)**
- **Spring Boot 3.5.x**
- **Spring Web / Validation / Security / Actuator**
- **Spring Data JPA**
- **PostgreSQL**
- **Springdoc OpenAPI (Swagger UI)**
- **JWT (`io.jsonwebtoken` / JJWT)**
- **Bucket4j + Caffeine** (rate limiting)
- **Lombok**
- **JUnit 5 + Spring Boot Test + Testcontainers**
- **Docker / Docker Compose** (runtime image + local Postgres helper)

## Getting Started

### Prerequisites

- **JDK 21** (required by `pom.xml` compiler target)
- **Docker** (optional, for local Postgres)

### 1) Start PostgreSQL (optional but recommended)

```bash
cd docker/postgres
docker compose up -d
```

### 2) Configure required environment variables

The required variables (documented in `.env.example`):

```bash
# Database
DB_URL=jdbc:postgresql://localhost:5432/gamilife_db
DB_USERNAME=postgres
DB_PASSWORD=admin

# JWT
JWT_SECRET=replace-with-a-secure-random-secret

# OAuth2 – Google
GOOGLE_CLIENT_ID=your-google-client-id
GOOGLE_CLIENT_SECRET=your-google-client-secret

# Frontend base URL (also used to build the OAuth2 redirect URI: <FRONTEND_URL>/oauth2/callback)
FRONTEND_URL=http://localhost:3000

# Brevo (transactional e-mail)
BREVO_API_KEY=your-brevo-api-key
```

### 3) Build the project

```bash
./mvnw clean package
```

### 4) Run the application

> **Note:** Spring Boot does not load environment variables automatically. Make sure all variables listed in `.env.example` are exported into your shell before running the application.

Dev-style run (matches `.run/Dev.run.xml` profile usage):

```bash
./mvnw -pl app spring-boot:run -Dspring-boot.run.profiles=dev
```

Or run the packaged JAR from `app/target/*-exec.jar`.

## Usage

### API base

- Base URL: `http://localhost:8080`
- Main REST namespace: `/api/v1/**`

### API documentation

In default config, Swagger UI is available at:

- `http://localhost:8080/swagger-ui/index.html`

### Example endpoints

- `POST /api/v1/auth/register`
- `POST /api/v1/auth/login`
- `PUT /api/v1/users/{userId}`
- `PATCH /api/v1/tasks/{taskId}`
- `POST /api/v1/groups`
- `PUT /api/v1/groups/{groupId}`
- `POST /api/v1/groups/{groupId}/tasks`
- `GET /api/v1/gamification-users/{userId}`
- `GET /api/v1/groups/{groupId}/shop/items`

### Build a production Docker image

```bash
docker build -t gamilife-backend .
```

The Dockerfile builds with Maven and runs the app on a Java 21 JRE image using the `prod` profile.

## Project Structure

```text
gamilife-backend/
├── pom.xml                      # Root aggregator (api, app, shared, modules)
├── Dockerfile
├── api/                         # Inter-module API contracts (DTOs/interfaces)
│   ├── auth-api/
│   ├── gamification-api/
│   ├── group-api/
│   ├── task-api/
│   └── user-api/
├── app/                         # Application bootstrap/composition module
│   └── src/main/
│       ├── java/pl/gamilife/app/StartUp.java
│       └── resources/application*.yml
├── shared/                      # Shared infrastructure/cross-cutting modules
│   ├── shared-kernel/           # Core abstractions, events, shared contracts
│   ├── shared-persistence/      # JPA and persistence base
│   └── shared-web/              # Security, filters, error handling, web helpers
├── modules/                     # Business/domain modules
│   ├── auth/
│   ├── communication/
│   ├── gamification/
│   ├── group/
│   ├── group-shop/
│   ├── group-task/
│   ├── pomodoro/
│   ├── task/
│   └── user/
└── docker/
    ├── postgres/docker-compose.yml
    └── sonarqube/docker-compose.yml
```

## Authors

- [Krzysztof Drapała](https://github.com/draps0n)
- [Jonasz Sójka](https://github.com/sonaszjojka)
- [Jan Wróblewski](https://github.com/WroJan25)

## License

This project is copyright © 2026 Krzysztof Drapała, Jonasz Sójka, and Jan Wróblewski, and is distributed under the **MIT License**. See the [LICENSE](LICENSE) file for full details.
