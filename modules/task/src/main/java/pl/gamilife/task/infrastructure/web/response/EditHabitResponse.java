package pl.gamilife.task.infrastructure.web.response;

import java.time.Duration;
import java.time.Instant;
import java.util.UUID;

public record EditHabitResponse(
        UUID habitId,
        Duration cycleLength,
        Integer currentStreak,
        Integer longestStreak,
        Instant finishedAt,
        Instant updated_at,
        Instant created_at
) {
}
