package pl.gamilife.task.application.edittaskforgrouptask;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.UUID;

public record EditTaskForGroupTaskResult(
        UUID taskId,
        String title,
        LocalDate deadlineDate,
        LocalTime deadlineTime,
        Integer categoryId,
        Integer difficultyId,
        String description
) {
}
