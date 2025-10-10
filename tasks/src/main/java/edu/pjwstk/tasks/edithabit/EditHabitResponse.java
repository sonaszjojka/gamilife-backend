package edu.pjwstk.tasks.edithabit;

import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.util.UUID;

public record EditHabitResponse(
        UUID habitId,
        Duration cycleLength,
        Integer currentStreak,
        Integer longestStreak,
        Boolean isAccepted,
        LocalDateTime acceptedDate,
        String declineMessage,
        Instant updated_at,
        Instant created_at
) {
}
