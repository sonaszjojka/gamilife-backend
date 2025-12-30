package pl.gamilife.task.application.findtasksbyids;

import java.io.Serializable;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.Collection;
import java.util.UUID;

public record FindTasksByIdsResult(
        Collection<TaskDto> tasks
) {
    public record TaskDto(UUID id,
                          String title,
                          String description,
                          UUID userId,
                          TaskCategoryDto category,
                          TaskDifficultyDto difficulty,
                          LocalDate deadlineDate,
                          LocalTime deadlineTime,
                          Instant completedAt
    ) {
    }

    public record TaskDifficultyDto(Integer id, String difficultyName) implements Serializable {
    }

    public record TaskCategoryDto(Integer id, String categoryName) implements Serializable {
    }
}
