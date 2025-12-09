package pl.gamilife.task.controllers.response;


import lombok.Builder;
import pl.gamilife.api.pomodoro.dto.PomodoroTaskDto;

import java.time.Duration;
import java.time.Instant;
import java.util.UUID;

@Builder
public record GetUserTasksDto(
        UUID taskId,
        String title,
        String description,
        Instant deadline,
        Integer categoryId,
        Integer difficultyId,
        Instant completedAt,
        String categoryName,
        String difficultyName,
        Boolean isGroupTask,
        UUID userId,
        PomodoroTaskDto pomodoro,
        TaskHabitDto taskHabit
) {
    @Builder
    public record TaskHabitDto(
            UUID habitId,
            Duration cycleLength,
            Integer currentStreak,
            Integer longestStreak,
            Instant finishedAt
    ) {
    }
}
