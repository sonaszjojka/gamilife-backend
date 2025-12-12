package pl.gamilife.task.infrastructure.web.response;


import lombok.Builder;
import pl.gamilife.task.domain.model.enums.TaskStatus;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.UUID;

@Builder
public record GetUserTasksDto(
        UUID taskId,
        String title,
        String description,
        LocalDate deadlineDate,
        LocalTime deadlineTime,
        Integer categoryId,
        Integer difficultyId,
        TaskStatus status,
        String categoryName,
        String difficultyName,
        Boolean isGroupTask,
        UUID userId
) {
    @Builder
    public record TaskHabitDto(
            UUID habitId,
            Integer cycleLength,
            Integer currentStreak,
            Integer longestStreak,
            Instant finishedAt
    ) {
    }
}
