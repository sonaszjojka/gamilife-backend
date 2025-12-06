package pl.gamilife.api.task.dto;

import lombok.Builder;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.UUID;


@Builder
public record TaskDto(UUID id, String title, LocalDateTime startTime, LocalDateTime endTime, TaskCategoryDto category,
                      TaskDifficultyDto difficulty, UUID userId, LocalDateTime completedAt, HabitDto habitTask,
                      String description) implements Serializable {

    public record TaskDifficultyDto(Integer id, String difficultyName) implements Serializable {
    }

    public record TaskCategoryDto(Integer id, String categoryName) implements Serializable {

    }


    public record HabitDto(UUID id) implements Serializable {
    }

}