package pl.gamilife.api.task.dto;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

public record TaskForGroupTaskRequestDto(
        String title,
        LocalDate deadlineDate,
        LocalTime deadlineTime,
        LocalDateTime currentGroupDateTime,
        Integer categoryId,
        Integer difficultyId,
        Boolean completed,
        String description
) {
}
