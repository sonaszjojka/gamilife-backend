package pl.gamilife.api.task.dto;

import java.time.Instant;

public record TaskForGroupTaskRequestDto(
        String title,
        Instant deadline,
        Integer categoryId,
        Integer difficultyId,
        Boolean completed,
        String description
) {
}
