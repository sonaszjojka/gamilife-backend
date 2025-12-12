package pl.gamilife.api.task.dto;

import java.io.Serializable;
import java.time.LocalDate;
import java.util.UUID;

public record HabitDto(
        UUID id,
        String title,
        String description,
        UUID userId,
        TaskCategoryDto category,
        TaskDifficultyDto difficulty,
        LocalDate currentDeadline,
        boolean canBeWorkedOn
) implements Serializable {

    public record TaskDifficultyDto(Integer id, String difficultyName) implements Serializable {
    }

    public record TaskCategoryDto(Integer id, String categoryName) implements Serializable {
    }

}