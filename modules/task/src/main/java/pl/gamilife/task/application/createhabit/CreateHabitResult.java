package pl.gamilife.task.application.createhabit;

import java.time.Instant;
import java.time.LocalDate;

import java.util.UUID;

public record CreateHabitResult(
        UUID habitId,
        Integer cycleLength,
        LocalDate currentDeadline,
        Integer currentStreak,
        Integer longestStreak,
        Instant created_at,
        Instant updated_at
) {
}
