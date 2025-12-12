package pl.gamilife.task.application.createtaskforgrouptask;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.UUID;

public record CreateTaskForGroupTaskResult(
        UUID taskId,
        String title,
        LocalDate deadlineDate,
        LocalTime deadlineTime,
        Integer categoryId,
        Integer difficultyId,
        String description
) {
}
