package edu.pjwstk.tasks.edittask;

import lombok.Builder;

import java.time.Duration;
import java.time.Instant;
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
        UUID taskHabitId,
        UUID previousTaskId,
        String description
) {
}
