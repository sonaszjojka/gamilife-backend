package edu.pjwstk.tasks.application.edittask;

import lombok.Builder;

import java.time.LocalDateTime;
import java.util.UUID;

@Builder
public record EditTaskResponse(
        UUID taskId,
        String title,
        LocalDateTime startTime,
        LocalDateTime endTime,
        Integer categoryId,
        Integer difficultyId,
        UUID userId,
        LocalDateTime completedAt,
        String description
) {
}
