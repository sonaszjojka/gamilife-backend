package pl.gamilife.task.application.createhabit;

import java.time.Duration;
import java.time.Instant;
import java.util.UUID;

public record CreateHabitResult(
        UUID habitId,
        Duration cycleLength,
        Integer currentStreak,
        Integer longestStreak,
        Instant created_at,
        Instant updated_at
) {
}
