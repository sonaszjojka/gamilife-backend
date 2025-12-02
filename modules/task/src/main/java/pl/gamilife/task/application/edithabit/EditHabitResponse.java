package pl.gamilife.task.application.edithabit;

import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.util.UUID;

public record EditHabitResponse(
        UUID habitId,
        Duration cycleLength,
        Integer currentStreak,
        Integer longestStreak,
        LocalDateTime acceptedDate,
        Instant updated_at,
        Instant created_at
) {
}
