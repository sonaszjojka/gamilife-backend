package pl.gamilife.task.application.createtask;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalTime;
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
