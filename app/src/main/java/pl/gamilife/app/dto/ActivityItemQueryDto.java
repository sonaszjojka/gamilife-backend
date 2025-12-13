package pl.gamilife.app.dto;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.UUID;

public record ActivityItemQueryDto(
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
