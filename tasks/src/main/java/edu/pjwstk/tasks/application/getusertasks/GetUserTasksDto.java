package edu.pjwstk.tasks.application.getusertasks;


import edu.pjwstk.api.pomodoro.PomodoroTaskDto.PomodoroTaskDto;
import lombok.Builder;

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
        PomodoroTaskDto pomodoro,
        TaskHabitDto taskHabit






) {
}
