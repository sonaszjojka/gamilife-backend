package edu.pjwstk.tasks.createtask;

import lombok.Builder;

import java.time.LocalDateTime;
import java.util.UUID;

@Builder
public record CreateTaskResponse(
        UUID taskId,
        String title,
        LocalDateTime startTime,
        LocalDateTime endTime,
        Integer categoryId,
        Integer difficultyId,
        UUID userId,
        LocalDateTime completedAt,
        UUID habitTaskId,
        UUID previousTaskId,
        String description
) {

}
