package pl.gamilife.task.application.findtaskbyid;

import lombok.Builder;

import java.io.Serializable;
import java.time.Instant;
import java.util.UUID;

@Builder
public record FindTaskByIdResult(
        UUID id,
        String title,
        String description,
        UUID userId,
        TaskCategoryDto category,
        TaskDifficultyDto difficulty,
        Instant deadline,
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
