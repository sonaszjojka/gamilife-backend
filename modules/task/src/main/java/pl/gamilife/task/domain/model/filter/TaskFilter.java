package pl.gamilife.task.domain.model.filter;

import java.util.UUID;

public record TaskFilter(
        UUID userId,
        Integer categoryId,
        Integer difficultyId,
        Boolean isGroupTask,
        Boolean isCompleted
) {
}
