package pl.gamilife.task.controllers.response;

import java.time.Duration;
import java.time.Instant;
import java.util.UUID;

public record CreateHabitResponse(
        UUID habitId,
        Duration cycleLength,
        Integer currentStreak,
        Integer longestStreak,
        Instant created_at,
        Instant updated_at
) {
}
