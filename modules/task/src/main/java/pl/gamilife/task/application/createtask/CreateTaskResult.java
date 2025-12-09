package pl.gamilife.task.controllers.response;

import java.time.Instant;
<<<<<<<< HEAD:modules/task/src/main/java/pl/gamilife/task/application/createtask/CreateTaskResult.java
import java.time.LocalDate;
import java.time.LocalTime;
========
>>>>>>>> ffd61dcb (chore: move requests and responses to infra):modules/task/src/main/java/pl/gamilife/task/controllers/response/CreateTaskResponse.java
import java.util.UUID;

public record CreateTaskResult(
        UUID taskId,
        String title,
        String description,
        LocalDate deadlineDate,
        LocalTime deadlineTime,
        Integer categoryId,
        Integer difficultyId,
        UUID userId,
        Instant completedAt
) {
}
