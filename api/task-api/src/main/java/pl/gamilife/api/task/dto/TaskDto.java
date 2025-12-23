package pl.gamilife.api.task.dto;

import java.io.Serializable;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.UUID;

public record TaskDto(
        UUID id,
        String title,
        String description,
        UUID userId,
        TaskCategoryDto category,
        TaskDifficultyDto difficulty,
        LocalDate deadlineDate,
        LocalTime deadlineTime,
        Instant completedAt
) implements Serializable {

    public record TaskDifficultyDto(Integer id, String difficultyName) implements Serializable {
    }

    public record TaskCategoryDto(Integer id, String categoryName) implements Serializable {
    }

}