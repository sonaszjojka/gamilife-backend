package pl.gamilife.task.application.edittaskforgrouptask;

import lombok.Builder;

import java.time.Instant;
import java.util.UUID;

@Builder
public record EditTaskForGroupTaskResult(
        UUID taskId,
        String title,
        Instant deadline,
        Integer categoryId,
        Integer difficultyId,
        String description
) {
}
