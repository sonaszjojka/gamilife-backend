package pl.gamilife.task.application.createtaskforgrouptask;

import lombok.Builder;

import java.time.Instant;
import java.util.UUID;

@Builder
public record CreateTaskForGroupTaskResult(
        UUID taskId,
        String title,
        Instant deadline,
        Integer categoryId,
        Integer difficultyId,
        String description
) {
}
