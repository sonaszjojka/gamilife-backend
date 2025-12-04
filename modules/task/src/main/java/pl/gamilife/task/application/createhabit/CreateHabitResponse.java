package pl.gamilife.task.application.createhabit;

import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.util.UUID;

public record CreateHabitResponse(
        UUID habitId,
        Duration cycleLength,
        Integer currentStreak,
        Integer longestStreak,
        LocalDateTime acceptedDate,
        Instant updated_at,
        Instant created_at

) {
}
