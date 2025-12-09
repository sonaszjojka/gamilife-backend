package pl.gamilife.task.application.createtask;

import lombok.Builder;

import java.time.Instant;
import java.util.UUID;

@Builder
public record CreateTaskResult(
        UUID taskId,
        String title,
        String description,
        Instant deadline,
        Integer categoryId,
        Integer difficultyId,
        UUID userId,
        Instant completedAt

) {

}
