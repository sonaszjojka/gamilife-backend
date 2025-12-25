package pl.gamilife.app.dto;

import java.time.ZoneId;
import java.util.UUID;

public record ActivityItemWithPomodoroFilter(
        UUID userId,
        ZoneId zoneId,
        String title,
        Boolean workable,
        Boolean pomodoro
) {
}
