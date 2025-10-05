package edu.pjwstk.tasks.createhabit;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.UUID;

public record CreateHabitResponse(
        UUID habitId,
        Duration cycleLength,
        Integer currentStreak,
        Integer longestStreak,
        LocalDateTime createdAt,
        Boolean isAccepted,
        LocalDateTime acceptedDate,
        String declineMessage,
        LocalDateTime lastEdit

) {
}
