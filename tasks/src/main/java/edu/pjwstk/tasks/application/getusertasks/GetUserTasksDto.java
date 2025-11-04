package edu.pjwstk.tasks.application.getusertasks;

import lombok.Builder;

import java.time.LocalDateTime;
import java.util.UUID;

@Builder
public record GetUserTasksDto(
        UUID taskId,
        String title,
        LocalDateTime startTime,
        LocalDateTime endTime,
        Integer categoryId,
        Integer difficultyId,
        LocalDateTime completedAt,
        UUID habitTaskId,
        UUID previousTaskId,
        String description
) {
}
