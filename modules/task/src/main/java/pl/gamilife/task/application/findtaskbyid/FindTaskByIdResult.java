package pl.gamilife.task.application.findtaskbyid;

import java.io.Serializable;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.UUID;

public record FindTaskByIdResult(
        UUID id,
        String title,
        String description,
        UUID userId,
        TaskCategoryDto category,
        TaskDifficultyDto difficulty,
        LocalDate deadlineDate,
        LocalTime deadlineTime,
        Instant completedAt,
        HabitDto habitTask
) {
    public record TaskDifficultyDto(Integer id, String difficultyName) implements Serializable {
    }

    public record TaskCategoryDto(Integer id, String categoryName) implements Serializable {
    }

    public record HabitDto(UUID id) implements Serializable {
    }
}
