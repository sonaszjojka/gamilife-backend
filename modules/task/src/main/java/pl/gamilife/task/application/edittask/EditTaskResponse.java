package pl.gamilife.task.application.edittask;

import lombok.Builder;

import java.time.Instant;
import java.util.UUID;

@Builder
public record EditTaskResponse(
        UUID taskId,
        String title,
        Instant deadline,
        Integer categoryId,
        Integer difficultyId,
        UUID userId,
        Instant completedAt,
        String description
) {
}
