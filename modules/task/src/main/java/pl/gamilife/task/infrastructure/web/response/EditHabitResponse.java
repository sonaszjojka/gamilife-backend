package pl.gamilife.task.infrastructure.web.response;

import java.time.Instant;
import java.util.UUID;

public record EditHabitResponse(
        UUID habitId,
        Integer cycleLength,
        Integer currentStreak,
        Integer longestStreak,
        Instant updated_at,
        Instant created_at
) {
}
