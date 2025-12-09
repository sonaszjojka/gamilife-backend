package pl.gamilife.task.controllers.response;

import lombok.Builder;

import java.time.Instant;
import java.util.UUID;

@Builder
public record CreateTaskResponse(
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
