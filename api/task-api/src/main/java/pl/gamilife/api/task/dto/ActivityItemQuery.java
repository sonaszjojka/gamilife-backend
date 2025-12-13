package pl.gamilife.api.task.dto;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.UUID;

public record ActivityItemQuery(
        UUID userId,
        ZoneId zoneId,
        String title,
        Integer categoryId,
        Integer difficultyId,
        LocalDate startDate,
        LocalDate endDate,
        Integer page,
        Integer size
) {
}
