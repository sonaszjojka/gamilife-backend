package pl.gamilife.task.controllers.response;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.UUID;

public record EditTaskResult(
        UUID taskId,
        String title,
        LocalDate deadlineDate,
        LocalTime deadlineTime,
        Integer categoryId,
        Integer difficultyId,
        UUID userId,
        Instant completedAt,
        String description
) {
}
