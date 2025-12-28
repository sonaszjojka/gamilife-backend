FROM maven:3.9.5-eclipse-temurin-21 AS build

WORKDIR /app

COPY pom.xml .

COPY api/pom.xml api/
COPY shared/pom.xml shared/
COPY modules/pom.xml modules/
COPY app/pom.xml app/

COPY api/auth-api/pom.xml api/auth-api/
COPY api/gamification-api/pom.xml api/gamification-api/
COPY api/group-api/pom.xml api/group-api/
COPY api/group-shop-api/pom.xml api/group-shop-api/
COPY api/task-api/pom.xml api/task-api/
COPY api/user-api/pom.xml api/user-api/

COPY shared/shared-kernel/pom.xml shared/shared-kernel/
COPY shared/shared-persistence/pom.xml shared/shared-persistence/
COPY shared/shared-web/pom.xml shared/shared-web/

COPY modules/auth/pom.xml modules/auth/
COPY modules/communication/pom.xml modules/communication/
COPY modules/gamification/pom.xml modules/gamification/
COPY modules/group/pom.xml modules/group/
COPY modules/group-shop/pom.xml modules/group-shop/
COPY modules/group-task/pom.xml modules/group-task/
COPY modules/pomodoro/pom.xml modules/pomodoro/
COPY modules/task/pom.xml modules/task/
COPY modules/user/pom.xml modules/user/

RUN mvn -B dependency:go-offline --no-transfer-progress

COPY . .

RUN mvn -B -DskipTests --no-transfer-progress package -Pprod

FROM eclipse-temurin:21-jre-alpine

WORKDIR /app

COPY --from=build /app/app/target/*.jar app.jar

ENV SPRING_PROFILES_ACTIVE=prod
ENV TZ=Europe/Warsaw

ENTRYPOINT ["java", "-XX:+UseContainerSupport", "-XX:MaxRAMPercentage=80", "-jar", "app.jar"]
