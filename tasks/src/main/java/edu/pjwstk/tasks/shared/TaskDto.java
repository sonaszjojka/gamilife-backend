package edu.pjwstk.tasks.shared;

import lombok.Builder;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.UUID;

/**
 * DTO for {@link edu.pjwstk.tasks.entity.Task}
 */
@Builder
public record TaskDto(UUID id, String title, LocalDateTime startTime, LocalDateTime endTime, TaskCategoryDto category, TaskDifficultyDto difficulty, UUID userId, LocalDateTime completedAt, HabitDto habitTask, TaskDto previousTask, String description) implements Serializable {
    /**
     * DTO for {@link edu.pjwstk.tasks.entity.TaskDifficulty}
     */
    public record TaskDifficultyDto(Integer id) implements Serializable {
    }

    /**
     * DTO for {@link edu.pjwstk.tasks.entity.Habit}
     */
    public record HabitDto(UUID id) implements Serializable {
    }

    /**
     * DTO for {@link edu.pjwstk.tasks.entity.Task}
     */
    public record PreviousTaskDto(UUID id) implements Serializable {
    }
}