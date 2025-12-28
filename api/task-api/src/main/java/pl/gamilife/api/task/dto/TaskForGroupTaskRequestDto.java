package pl.gamilife.api.task.dto;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;

public record TaskForGroupTaskRequestDto(
        String title,
        LocalDate deadlineDate,
        Boolean removeDeadlineTime,
        LocalTime deadlineTime,
        ZoneId currentGroupTimezone,
        Integer categoryId,
        Integer difficultyId,
        Boolean completed,
        Boolean removeDescription,
        String description
) {
}
