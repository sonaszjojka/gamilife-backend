FROM maven:3.9.5-eclipse-temurin-17 AS build

WORKDIR /app

COPY pom.xml .

COPY application/pom.xml application/
COPY auth/pom.xml auth/
COPY budget/pom.xml budget/
COPY common-api/pom.xml common-api/
COPY common-core/pom.xml common-core/
COPY common-web/pom.xml common-web/
COPY email-sender/pom.xml email-sender/
COPY gamification/pom.xml gamification/
COPY group-shop/pom.xml group-shop/
COPY group-tasks/pom.xml group-tasks/
COPY groups/pom.xml groups/
COPY pomodoro/pom.xml pomodoro/
COPY tasks/pom.xml tasks/
COPY user/pom.xml user/

RUN mvn -B dependency:go-offline --no-transfer-progress

COPY . .

RUN mvn -B -DskipTests --no-transfer-progress package -Pprod

FROM eclipse-temurin:17-jre-alpine

WORKDIR /app

COPY --from=build /app/target/*.jar app.jar

ENV SPRING_PROFILES_ACTIVE=prod
ENV TZ=Europe/Warsaw

ENTRYPOINT ["java", "-XX:+UseContainerSupport", "-XX:MaxRAMPercentage=80", "-jar", "app.jar"]
