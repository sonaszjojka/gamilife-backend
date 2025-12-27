package pl.gamilife.app.dto.activity;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.UUID;

public record ActivityItemFilter(
        UUID userId,
        ZoneId zoneId,
        String title,
        Integer categoryId,
        Integer difficultyId,
        LocalDate startDate,
        LocalDate endDate,
        Boolean workable,
        Boolean pomodoro
) {
}
