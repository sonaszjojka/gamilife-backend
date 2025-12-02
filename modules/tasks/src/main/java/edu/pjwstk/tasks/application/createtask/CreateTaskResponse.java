package edu.pjwstk.tasks.application.createtask;

import lombok.Builder;

import java.time.LocalDateTime;
import java.util.UUID;

@Builder
public record CreateTaskResponse(
        UUID taskId,
        String title,
        String description,
        LocalDateTime startTime,
        LocalDateTime endTime,
        Integer categoryId,
        Integer difficultyId,
        UUID userId,
        LocalDateTime completedAt

) {

}
