package edu.pjwstk.tasks.application.getusertasks;

import lombok.Builder;

import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.util.UUID;

@Builder
public record GetUserTasksDto(
        UUID taskId,
        String title,
        String description,
        LocalDateTime startTime,
        LocalDateTime endTime,
        Integer categoryId,
        Integer difficultyId,
        LocalDateTime completedAt,
        String categoryName,
        String difficultyName,
        Boolean isGroupTask,
        UUID userId,
        UUID pomodoroId,
        Integer workCyclesNeeded,
        Integer workCyclesCompleted,
        Instant createdAt,
        UUID habitId,
        Duration cycleLength,
        Integer currentStreak,
        Integer longestStreak,
        Boolean isAccepted,
        LocalDateTime acceptedDate,
        String declineMessage






) {
}
