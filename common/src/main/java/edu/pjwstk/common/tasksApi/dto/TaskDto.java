package edu.pjwstk.common.tasksApi.dto;

import lombok.Builder;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.UUID;


@Builder
public record TaskDto(UUID id, String title, LocalDateTime startTime, LocalDateTime endTime, TaskCategoryDto category, TaskDifficultyDto difficulty, UUID userId, LocalDateTime completedAt, HabitDto habitTask, TaskDto previousTask, String description) implements Serializable {

    public record TaskDifficultyDto(Integer id) implements Serializable {
    }


    public record HabitDto(UUID id) implements Serializable {
    }

    public record PreviousTaskDto(UUID id) implements Serializable {
    }
}